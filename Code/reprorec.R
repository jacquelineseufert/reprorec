# reprorec — consolidated core + utilities ------------------------------------
# Dependencies: httr2, jsonlite, digest, fs, tools, readr (for schema diffs), utils
# Optional: cli (for nicer messages)
#
# Install once:
# install.packages(c("httr2","jsonlite","digest","fs","tools","readr","cli"))

# ──────────────────────────────────────────────────────────────────────────────
# Internal helpers (not exported)
# ──────────────────────────────────────────────────────────────────────────────

# Compute SHA-256 hash of a local file (tamper-evident fingerprint).
.hash_file <- function(path) digest::digest(file = path, algo = "sha256")

# Derive the receipt path for a given local data file.
.ReceiptPath <- function(file_path) {
  fs::dir_create("receipts")
  fs::path("receipts", paste0(fs::path_file(file_path), ".receipt.json"))
}

# Null/empty coalesce.
`%||%` <- function(a, b) if (is.null(a) || is.na(a) || identical(a, "")) b else a

# RFC3339 UTC timestamp.
.utc_now <- function() format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

# Write a JSON file (pretty) and return the path invisibly.
.write_receipt <- function(obj, path_out) {
  fs::dir_create(fs::path_dir(path_out))
  jsonlite::write_json(obj, path_out, auto_unbox = TRUE, pretty = TRUE)
  invisible(path_out)
}

# Message helper: if cli is present, use it; else basic message().
.msg <- function(kind = "info", text = "") {
  if (requireNamespace("cli", quietly = TRUE)) {
    key <- switch(kind, info = ">", ok = "v", warn = "!", err = "x", ">")
    cli::cli_inform(setNames(list(text), key))
  } else {
    prefix <- switch(kind, info = "[i]", ok = "[✓]", warn = "[!]", err = "[x]", "[i]")
    message(prefix, " ", text)
  }
}

# ──────────────────────────────────────────────────────────────────────────────
# Public API
# ──────────────────────────────────────────────────────────────────────────────

#' Download a file and write a reproducibility receipt
#'
#' Downloads any file from `url`, saves it to `dest` (or under `data/` if `dest` is
#' NULL), computes a SHA-256 hash, and writes a JSON receipt under `receipts/`.
#'
#' The receipt stores: URL, DOI (optional), timestamp (UTC), HTTP metadata
#' (status, type, content-length, etag, last-modified when available),
#' and local file info (path, size, extension, SHA-256 hash).
#'
#' @param url Character. Source URL to download (CSV, PDF, ZIP, TSV.gz, ...).
#' @param dest Optional local path. If NULL, file is saved as data/<basename(url)>.
#' @param doi Optional DOI string to record in the receipt.
#' @param quiet Logical; if TRUE suppresses messages.
#' @return Invisible list(file = <local path>, receipt = <receipt path>)
#' @export
rr_fetch <- function(url, dest = NULL, doi = NULL, quiet = FALSE) {
  if (!quiet) .msg("info", paste0("Fetching ", url))
  
  # Attempt a HEAD request to capture HTTP metadata (some servers disallow HEAD).
  head_meta <- list(status = NA_integer_, type = NA_character_, len = NA_real_,
                    etag = NA_character_, last_mod = NA_character_)
  req <- httr2::request(url)
  head_resp <- try(httr2::req_head(req) |> httr2::req_perform(), silent = TRUE)
  if (!inherits(head_resp, "try-error")) {
    head_meta$status   <- httr2::resp_status(head_resp)
    head_meta$type     <- httr2::resp_header(head_resp, "content-type")
    head_meta$len      <- suppressWarnings(as.numeric(httr2::resp_header(head_resp, "content-length")))
    head_meta$etag     <- httr2::resp_header(head_resp, "etag")
    head_meta$last_mod <- httr2::resp_header(head_resp, "last-modified")
  }
  
  # Choose destination path.
  if (is.null(dest)) {
    fs::dir_create("data")
    name <- basename(httr2::url_parse(url)$path)
    if (is.na(name) || name == "" || name == "/") {
      name <- paste0("download-", format(Sys.time(), "%Y%m%d-%H%M%S"))
    }
    dest <- fs::path("data", name)
  } else {
    fs::dir_create(fs::path_dir(dest))
  }
  
  # GET body → file (stream).
  resp <- try(httr2::req_perform(req), silent = TRUE)
  if (inherits(resp, "try-error")) stop("Download failed. Check the URL or your connection.")
  writeBin(httr2::resp_body_raw(resp), dest)
  
  # Local facts.
  size <- as.numeric(fs::file_info(dest)$size)
  sha  <- tolower(.hash_file(dest))
  
  # Compose receipt.
  rec <- list(
    schema_version = 1L,
    tool           = "reprorec",
    tool_version   = "0.1.0",
    timestamp_utc  = .utc_now(),
    source = list(
      url            = url,
      doi            = doi,
      http_status    = head_meta$status %||% httr2::resp_status(resp),
      content_type   = head_meta$type %||% httr2::resp_header(resp, "content-type"),
      content_length = (if (!is.na(head_meta$len)) head_meta$len else size),
      etag           = head_meta$etag,
      last_modified  = head_meta$last_mod
    ),
    file = list(
      path       = fs::path_abs(dest),
      size_bytes = size,
      sha256     = sha,
      extension  = tools::file_ext(dest)
    ),
    notes = NULL
  )
  
  rcp <- .ReceiptPath(dest)
  .write_receipt(rec, rcp)
  
  if (!quiet) .msg("ok", paste0("Saved file: ", dest, " and receipt: ", rcp))
  invisible(list(file = dest, receipt = rcp))
}

#' Verify a receipt against the local file or the current remote file
#'
#' Recomputes the SHA-256 and compares to the receipt’s hash.
#' * `mode = "local"`: hash the local file at `rec$file$path`.
#' * `mode = "remote"`: re-download from `rec$source$url` to a temp file and hash.
#'
#' @param receipt_path Path to a `*.receipt.json` written by `rr_fetch()`.
#' @param mode Either `"local"` or `"remote"`.
#' @return Invisible list(ok, expected, got, mode)
#' @export
rr_verify <- function(receipt_path, mode = c("local","remote")) {
  mode <- match.arg(mode)
  if (!fs::file_exists(receipt_path)) stop("Receipt not found: ", receipt_path)
  rec <- jsonlite::read_json(receipt_path, simplifyVector = TRUE)
  
  expected <- tolower(rec$file$sha256)
  
  if (mode == "local") {
    if (!fs::file_exists(rec$file$path)) stop("Local file missing: ", rec$file$path)
    got <- tolower(.hash_file(rec$file$path))
    ok  <- identical(got, expected)
    .msg("info", if (ok) "Dataset unchanged (local)" else "Local file differs from receipt")
    return(invisible(list(ok = ok, expected = expected, got = got, mode = "local")))
  }
  
  # Remote verification: download to temp and compare hash.
  tmp <- fs::file_temp(ext = rec$file$extension %||% "")
  on.exit(fs::file_delete(tmp), add = TRUE)
  resp <- try(httr2::request(rec$source$url) |> httr2::req_perform(), silent = TRUE)
  if (inherits(resp, "try-error")) stop("Remote fetch failed for: ", rec$source$url)
  writeBin(httr2::resp_body_raw(resp), tmp)
  got <- tolower(.hash_file(tmp))
  ok  <- identical(got, expected)
  .msg("info", if (ok) "Remote version unchanged" else "Remote version changed since receipt")
  invisible(list(ok = ok, expected = expected, got = got, mode = "remote"))
}

#' Take a timestamped snapshot and append a ledger row
#'
#' Saves the file with a timestamp in the filename (e.g., `code_YYYY-mm-dd_HHMM.ext`)
#' and appends a row to `receipts/_ledger.csv` capturing URL, path, timestamp, size and hash.
#'
#' @param url Source URL.
#' @param code Short identifier (used in the filename and ledger).
#' @param stamp_fmt strptime format for the timestamp portion (default "%Y-%m-%d_%H%M").
#' @return A data.frame row with snapshot metadata (invisible).
#' @export
rr_snapshot <- function(url, code, stamp_fmt = "%Y-%m-%d_%H%M") {
  fs::dir_create("data")
  bn  <- basename(httr2::url_parse(url)$path)
  ext <- if (nzchar(bn)) bn else "download.bin"
  
  # Build destination with preserved extension(s) like ".tsv.gz".
  dest <- fs::path(
    "data",
    sprintf("%s_%s.%s", code, format(Sys.time(), stamp_fmt), sub("^.*?\\.(.*)$", "\\1", ext))
  )
  
  out <- rr_fetch(url, dest = dest, doi = NULL, quiet = TRUE)
  
  # Append to ledger CSV.
  rec <- jsonlite::read_json(out$receipt, simplifyVector = TRUE)
  row <- data.frame(
    code          = code,
    url           = rec$source$url,
    file          = out$file,
    receipt       = out$receipt,
    timestamp_utc = rec$timestamp_utc,
    size_bytes    = rec$file$size_bytes,
    sha256        = rec$file$sha256,
    stringsAsFactors = FALSE
  )
  fs::dir_create("receipts")
  ledger_path <- fs::path("receipts", "_ledger.csv")
  if (fs::file_exists(ledger_path)) {
    suppressWarnings(write.table(row, ledger_path, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE))
  } else {
    write.csv(row, ledger_path, row.names = FALSE)
  }
  invisible(row)
}

#' Read the ledger of snapshots
#'
#' Convenience function to read `receipts/_ledger.csv`.
#' @return A data.frame (tibble if readr is available) with snapshot rows.
#' @export
rr_ledger <- function() {
  path <- fs::path("receipts", "_ledger.csv")
  if (!fs::file_exists(path)) stop("No ledger found at ", path)
  readr::read_csv(path, show_col_types = FALSE)
}

#' Show a simple schema diff between the last two snapshots of a code
#'
#' For CSV/TSV (optionally gzipped) snapshots, reports columns added/removed.
#' Row-level diffs are out of scope here and can be added later.
#'
#' @param code The snapshot code used in `rr_snapshot()`.
#' @return A list with previous/current filenames and column changes.
#' @export
rr_diff_schema_latest <- function(code) {
  led <- rr_ledger()
  sub <- subset(led, code == !!code)
  if (nrow(sub) < 2) stop("Need at least two snapshots for code: ", code)
  sub <- sub[order(sub$timestamp_utc), ]
  a <- tail(sub, 2)[1, "file", drop = TRUE]
  b <- tail(sub, 1)[1, "file", drop = TRUE]
  
  # Pick delimiter by extension; readr auto-handles .gz.
  delim_a <- if (grepl("\\.tsv(\\.gz)?$", a, ignore.case = TRUE)) "\t" else ","
  delim_b <- if (grepl("\\.tsv(\\.gz)?$", b, ignore.case = TRUE)) "\t" else ","
  old <- readr::read_delim(a, delim = delim_a, show_col_types = FALSE, guess_max = 100000)
  new <- readr::read_delim(b, delim = delim_b, show_col_types = FALSE, guess_max = 100000)
  
  cols_added   <- setdiff(names(new), names(old))
  cols_removed <- setdiff(names(old), names(new))
  
  list(
    code = code,
    file_prev = basename(a),
    file_curr = basename(b),
    columns_added = cols_added,
    columns_removed = cols_removed,
    note = "Row-level diffs can be added later with keyed joins."
  )
}

#' Append a single receipt to a simple ledger (alternative to rr_snapshot)
#'
#' Useful when `rr_fetch()` was used directly with a custom `dest`.
#' @param receipt Path to a receipt JSON to record.
#' @return Nothing (writes/updates `receipts/_ledger.csv`).
#' @export
rr_log <- function(receipt) {
  rec <- jsonlite::read_json(receipt, simplifyVector = TRUE)
  row <- data.frame(
    file = basename(rec$file$path),
    url  = rec$source$url,
    sha  = rec$file$sha256,
    size = rec$file$size_bytes,
    time = rec$timestamp_utc
  )
  fs::dir_create("receipts")
  logf <- "receipts/_ledger.csv"
  if (fs::file_exists(logf)) {
    readr::write_csv(row, logf, append = TRUE)
  } else {
    readr::write_csv(row, logf)
  }
  invisible(NULL)
}

#' Sign a receipt with GPG (detached ASCII signature)
#'
#' Produces `<receipt>.asc` using your private key. Requires GnuPG (`gpg`) to be
#' installed. If GnuPG is not on PATH, pass `gpg_path` explicitly.
#'
#' @param receipt_path Path to a receipt JSON.
#' @param gpg_key Optional user id or fingerprint (e.g., email) to select the key.
#' @param gpg_path Path to gpg executable (default points to a common Windows location).
#' @return The path to the created signature file.
#' @export
rr_sign <- function(receipt_path, gpg_key = NULL,
                    gpg_path = "C:/Program Files (x86)/GnuPG/bin/gpg.exe") {
  sig_path <- paste0(receipt_path, ".asc")
  args <- c("--armor", "--detach-sign", "--output", sig_path)
  if (!is.null(gpg_key)) args <- c(args, "--local-user", gpg_key)
  args <- c(args, receipt_path)
  out <- tryCatch(
    system2(gpg_path, args, stdout = TRUE, stderr = TRUE),
    warning = function(w) capture.output(w),
    error   = function(e) capture.output(e)
  )
  if (!file.exists(sig_path)) stop("Signing failed:\n", paste(out, collapse = "\n"))
  invisible(sig_path)
}

#' Append a receipt entry to a public log (CSV)
#'
#' Appends a row containing timestamp, sha256, URL, receipt filename, and signature
#' filename to `public-log/log.csv` (creates folders as needed).
#'
#' @param receipt_path Path to the JSON receipt.
#' @param signature_path Path to the `.asc` signature for the receipt.
#' @param log_path Path to the CSV log (default `public-log/log.csv`).
#' @return The path to the log file.
#' @export
rr_log_publish <- function(receipt_path, signature_path, log_path = "public-log/log.csv") {
  rec <- jsonlite::read_json(receipt_path, simplifyVector = TRUE)
  row <- data.frame(
    timestamp_utc = rec$timestamp_utc,
    sha256        = rec$file$sha256,
    url           = rec$source$url,
    receipt       = basename(receipt_path),
    signature     = basename(signature_path),
    stringsAsFactors = FALSE
  )
  fs::dir_create(fs::path_dir(log_path))
  if (file.exists(log_path)) readr::write_csv(row, log_path, append = TRUE)
  else                       readr::write_csv(row, log_path)
  invisible(log_path)
}

#' Compute a Merkle root over the public log’s sha256 column
#'
#' Produces a small JSON with the Merkle root and an update timestamp.
#' This can later be time-anchored (e.g., with OpenTimestamps).
#'
#' @param log_path Path to `public-log/log.csv`.
#' @param out_path Path to write the Merkle summary JSON (default `public-log/merkle.json`).
#' @return The Merkle root (character).
#' @export
rr_log_merkle <- function(log_path = "public-log/log.csv", out_path = "public-log/merkle.json") {
  dat <- readr::read_csv(log_path, show_col_types = FALSE)
  if (!"sha256" %in% names(dat)) stop("Log does not contain a 'sha256' column: ", log_path)
  
  # Simple (toy) Merkle tree over sha256 values; for production you may want a
  # dedicated merkle library that also emits per-leaf proofs.
  h <- function(x) digest::digest(x, algo = "sha256", serialize = FALSE)
  level <- vapply(dat$sha256, h, "")
  while (length(level) > 1) {
    if (length(level) %% 2 == 1) level <- c(level, level[length(level)]) # pad last
    pairs <- split(level, ceiling(seq_along(level)/2))
    level <- vapply(pairs, function(p) h(paste0(p, collapse = "")), "")
  }
  root <- level[1]
  fs::dir_create(fs::path_dir(out_path))
  jsonlite::write_json(
    list(merkle_root = root, updated = .utc_now()),
    out_path, auto_unbox = TRUE, pretty = TRUE
  )
  invisible(root)
}


# ---- Helpers to read snapshots (CSV/TSV, optionally gz) ----------------------

# Read a CSV/TSV (optionally .gz) and return a data.frame.
# - delim is auto-guessed from the file extension
# - col_types are inferred (with a reasonably high guess_max)
.read_tabular <- function(path) {
  stopifnot(file.exists(path))
  is_tsv <- grepl("\\.tsv(\\.gz)?$", path, ignore.case = TRUE)
  delim  <- if (is_tsv) "\t" else ","
  readr::read_delim(path, delim = delim, show_col_types = FALSE, guess_max = 100000)
}

# Get the last two snapshots for a code from the ledger (previous, current).
# Returns a named list with paths and timestamps.
.last_two_for_code <- function(code) {
  led <- rr_ledger()
  sub <- subset(led, code == !!code)
  if (nrow(sub) < 2) stop("Need at least two snapshots for code: ", code)
  sub <- sub[order(sub$timestamp_utc), ]
  list(
    prev_path = sub$file[nrow(sub) - 1],
    prev_time = sub$timestamp_utc[nrow(sub) - 1],
    curr_path = sub$file[nrow(sub)],
    curr_time = sub$timestamp_utc[nrow(sub)]
  )
}

# ---- Row/cell-level diff between the last two snapshots ----------------------

# Compare last two snapshots for a code at the row/cell level.
# keys: character vector of column names that identify a row (e.g., c("geo","na_item","unit","s_adj","time"))
# value_col: single column name with the numeric value to compare (e.g., "value")
# Returns a list: added_rows, removed_rows, value_changes, and a compact summary data.frame
rr_diff_rows_latest <- function(code, keys, value_col) {
  lt <- .last_two_for_code(code)
  old <- .read_tabular(lt$prev_path)
  new <- .read_tabular(lt$curr_path)
  
  # Basic checks
  if (!all(keys %in% names(old)) || !all(keys %in% names(new)))
    stop("Some keys not present in both snapshots.")
  if (!(value_col %in% names(old) && value_col %in% names(new)))
    stop("value_col not present in both snapshots.")
  
  # Added/removed rows via anti-joins on keys
  suppressPackageStartupMessages(requireNamespace("dplyr", quietly = TRUE))
  library(dplyr)
  
  added_rows   <- dplyr::anti_join(new, old, by = keys)
  removed_rows <- dplyr::anti_join(old, new, by = keys)
  
  # Value changes on matched keys
  common_old <- dplyr::semi_join(old, new, by = keys)
  common_new <- dplyr::semi_join(new, old, by = keys)
  
  # Restrict to keys + value only to avoid spurious joins on extra columns
  common_old2 <- dplyr::select(common_old, dplyr::all_of(c(keys, value_col)))
  common_new2 <- dplyr::select(common_new, dplyr::all_of(c(keys, value_col)))
  
  joined <- dplyr::inner_join(common_old2, common_new2, by = keys, suffix = c("_old","_new"))
  # Only retain rows where the value actually changed (NA-safe comparison)
  value_changes <- dplyr::filter(
    joined,
    !(is.na(.data[[paste0(value_col, "_old")]]) && is.na(.data[[paste0(value_col, "_new")]])) &
      (.data[[paste0(value_col, "_old")]] != .data[[paste0(value_col, "_new")]])
  )
  value_changes <- dplyr::mutate(value_changes,
                                 delta = .data[[paste0(value_col, "_new")]] - .data[[paste0(value_col, "_old")]])
  
  summary <- data.frame(
    code        = code,
    prev_time   = lt$prev_time,
    curr_time   = lt$curr_time,
    rows_added  = nrow(added_rows),
    rows_removed= nrow(removed_rows),
    values_changed = nrow(value_changes),
    stringsAsFactors = FALSE
  )
  
  list(
    summary        = summary,
    added_rows     = added_rows,
    removed_rows   = removed_rows,
    value_changes  = value_changes
  )
}


# ---- Human-readable changelog for the last two snapshots ---------------------

# Combine schema-level and row-level diffs into a compact textual changelog.
# keys/value_col are forwarded to rr_diff_rows_latest().
rr_changelog_latest <- function(code, keys, value_col) {
  # Schema diff (columns added/removed)
  schema <- try(rr_diff_schema_latest(code), silent = TRUE)
  
  # Row/cell diff
  rd <- rr_diff_rows_latest(code, keys = keys, value_col = value_col)
  
  lines <- character()
  lines <- c(lines, sprintf("Code: %s", code))
  lines <- c(lines, sprintf("Previous snapshot: %s", rd$summary$prev_time))
  lines <- c(lines, sprintf("Current  snapshot: %s", rd$summary$curr_time))
  if (!inherits(schema, "try-error")) {
    if (length(schema$columns_added) || length(schema$columns_removed)) {
      if (length(schema$columns_added))
        lines <- c(lines, sprintf("Schema: columns added: %s", paste(schema$columns_added, collapse = ", ")))
      if (length(schema$columns_removed))
        lines <- c(lines, sprintf("Schema: columns removed: %s", paste(schema$columns_removed, collapse = ", ")))
    } else {
      lines <- c(lines, "Schema: no column changes detected")
    }
  }
  
  lines <- c(lines, sprintf("Rows added:   %d", rd$summary$rows_added))
  lines <- c(lines, sprintf("Rows removed: %d", rd$summary$rows_removed))
  lines <- c(lines, sprintf("Values changed (on matched keys): %d", rd$summary$values_changed))
  
  list(
    text = paste(lines, collapse = "\n"),
    schema_diff = schema,
    row_diff = rd
  )
}



# ---- Revision triangle over multiple vintages --------------------------------

# Build a revision triangle for a single series across the most recent n_vintages.
# keys_filter: named list/vector to select one series (e.g., list(geo="BE", na_item="B1GQ", unit="CP_MEUR", s_adj="NSA"))
# time_col, value_col: column names in the long-form data
# returns a list with wide matrices (as data.frames) and a long stack for plotting
rr_revision_triangle <- function(code, keys_filter, time_col, value_col, n_vintages = 8) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(purrr)
  })
  
  led <- rr_ledger()
  sub <- subset(led, code == !!code)
  if (!nrow(sub)) stop("No snapshots for code: ", code)
  sub <- sub[order(sub$timestamp_utc), ]
  sub <- tail(sub, min(n_vintages, nrow(sub)))
  
  # read and filter each vintage
  read_one <- function(p) {
    df <- .read_tabular(p)
    # apply filters
    for (nm in names(keys_filter)) {
      if (!nm %in% names(df)) stop("Missing filter column in data: ", nm)
      df <- dplyr::filter(df, .data[[nm]] == keys_filter[[nm]])
    }
    # keep time and value
    if (!all(c(time_col, value_col) %in% names(df)))
      stop("time_col or value_col not found in file: ", p)
    dplyr::select(df, !!time_col, !!value_col)
  }
  
  series_list <- map(sub$file, read_one)
  # bind with vintage label
  long <- purrr::map2_dfr(series_list, sub$timestamp_utc, ~ dplyr::mutate(.x, vintage = .y),
                          .id = "order")
  long$order <- NULL
  
  # pivot to triangle: rows=vintage, cols=time, value=obs
  triangle_wide <- long |>
    tidyr::pivot_wider(names_from = !!sym(time_col), values_from = !!sym(value_col)) |>
    dplyr::arrange(vintage)
  
  # compute revisions
  vals <- triangle_wide |> dplyr::select(-vintage)
  m <- as.matrix(vals)
  
  # vs first vintage (row 1)
  rev_first <- sweep(m, 2, m[1, , drop = TRUE], FUN = "-")
  # vs previous vintage
  rev_prev <- m
  if (nrow(m) >= 2) {
    for (i in 2:nrow(m)) rev_prev[i, ] <- m[i, ] - m[i - 1, ]
    rev_prev[1, ] <- NA
  } else {
    rev_prev[,] <- NA
  }
  
  triangle_wide$`__row_id__` <- seq_len(nrow(triangle_wide))
  revisions_vs_first <- cbind(vintage = triangle_wide$vintage, as.data.frame(rev_first, check.names = FALSE))
  revisions_vs_prev  <- cbind(vintage = triangle_wide$vintage, as.data.frame(rev_prev,  check.names = FALSE))
  
  list(
    triangle_wide = triangle_wide[, c("vintage", setdiff(names(triangle_wide), c("vintage","__row_id__")))],
    revisions_vs_first = revisions_vs_first,
    revisions_vs_prev  = revisions_vs_prev,
    long = long
  )
}



