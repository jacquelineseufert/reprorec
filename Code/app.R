# app.R — Reproducibility Verifier & File Comparator
# Dependencies: shiny, jsonlite, digest, httr2, fs, DT, readr, dplyr, tidyr
# install.packages(c("shiny","jsonlite","digest","httr2","fs","DT","readr","dplyr","tidyr"))

suppressPackageStartupMessages({
  library(shiny)
  library(jsonlite)
  library(digest)
  library(httr2)
  library(fs)
  library(DT)
  library(readr)
  library(dplyr)
  library(tidyr)
})

# ------------------------ helpers (standalone) ------------------------

# coalesce-like helper for possibly-empty JSON fields
`%||%` <- function(a, b) if (is.null(a) || is.na(a) || identical(a, "")) b else a

# compute SHA-256 on a file
hash_file <- function(path) digest(file = path, algo = "sha256")

# read CSV/TSV (optionally .gz) with delimiter guessed from extension
read_tabular <- function(path) {
  stopifnot(file.exists(path))
  is_tsv <- grepl("\\.tsv(\\.gz)?$", path, ignore.case = TRUE)
  delim  <- if (is_tsv) "\t" else ","
  readr::read_delim(path, delim = delim, show_col_types = FALSE, guess_max = 100000)
}

# schema diff: columns added/removed
schema_diff <- function(old_df, new_df) {
  cols_added   <- setdiff(names(new_df), names(old_df))
  cols_removed <- setdiff(names(old_df), names(new_df))
  list(added = cols_added, removed = cols_removed)
}

# row/cell-level diff for tidy long tables
# keys: columns that identify a row; value_col: numeric or character column to compare
row_cell_diff <- function(old_df, new_df, keys, value_col) {
  if (!all(keys %in% names(old_df)) || !all(keys %in% names(new_df))) {
    stop("Keys not present in both files.")
  }
  if (!(value_col %in% names(old_df) && value_col %in% names(new_df))) {
    stop("Value column not present in both files.")
  }
  
  added_rows   <- dplyr::anti_join(new_df, old_df, by = keys)
  removed_rows <- dplyr::anti_join(old_df, new_df, by = keys)
  
  common_old <- dplyr::semi_join(old_df, new_df, by = keys)
  common_new <- dplyr::semi_join(new_df, old_df, by = keys)
  
  common_old2 <- dplyr::select(common_old, dplyr::all_of(c(keys, value_col)))
  common_new2 <- dplyr::select(common_new, dplyr::all_of(c(keys, value_col)))
  
  joined <- dplyr::inner_join(common_old2, common_new2, by = keys,
                              suffix = c("_old", "_new"))
  
  v_old <- joined[[paste0(value_col, "_old")]]
  v_new <- joined[[paste0(value_col, "_new")]]
  
  # NA-safe inequality
  changed_idx <- !(is.na(v_old) & is.na(v_new)) & (v_old != v_new)
  value_changes <- joined[changed_idx, , drop = FALSE]
  if (is.numeric(v_old) && is.numeric(v_new)) {
    value_changes$delta <- v_new[changed_idx] - v_old[changed_idx]
  }
  
  list(
    added_rows = added_rows,
    removed_rows = removed_rows,
    value_changes = value_changes,
    summary = data.frame(
      rows_added = nrow(added_rows),
      rows_removed = nrow(removed_rows),
      values_changed = nrow(value_changes)
    )
  )
}

# ----------------------------- UI -----------------------------------

ui <- fluidPage(
  tags$head(tags$title("Reproducibility Verifier")),
  h2("Reproducibility Verifier"),
  p("Two ways to check reproducibility:"),
  tags$ul(
    tags$li(strong("Verify with Receipt:"), " upload a receipt JSON (and optionally the local file)."),
    tags$li(strong("Compare Two Files:"), " upload two datasets and get an identical/changed verdict and a diff.")
  ),
  tabsetPanel(
    tabPanel(
      "Verify with Receipt",
      br(),
      fluidRow(
        column(
          4,
          fileInput("receipt", "Receipt (.receipt.json)", accept = c(".json"), multiple = FALSE),
          fileInput("datafile", "Local data file (optional, for local verification)", multiple = FALSE),
          actionButton("check_local",  "Run local verification"),
          actionButton("check_remote", "Run remote verification"),
          checkboxInput("show_raw", "Show raw receipt JSON", value = FALSE)
        ),
        column(
          8,
          h4("Receipt metadata"),
          DTOutput("receipt_table"),
          verbatimTextOutput("receipt_note"),
          hr(),
          h4("Verification results"),
          verbatimTextOutput("local_result"),
          verbatimTextOutput("remote_result"),
          conditionalPanel("input.show_raw == true",
                           hr(),
                           h4("Raw receipt JSON"),
                           tags$pre(style = "white-space: pre-wrap;", textOutput("receipt_json"))
          )
        )
      )
    ),
    tabPanel(
      "Compare Two Files",
      br(),
      fluidRow(
        column(
          4,
          fileInput("fileA", "File A", multiple = FALSE),
          fileInput("fileB", "File B", multiple = FALSE),
          textInput("keys", "Key columns (comma-separated)", placeholder = "e.g. geo,s_adj,na_item,unit,time"),
          textInput("value_col", "Value column (for cell-level compare)", placeholder = "e.g. value"),
          actionButton("compare", "Compare")
        ),
        column(
          8,
          h4("Hash comparison"),
          verbatimTextOutput("hash_compare"),
          hr(),
          h4("Schema diff"),
          verbatimTextOutput("schema_compare"),
          hr(),
          h4("Row/cell-level changes"),
          verbatimTextOutput("rowcell_summary"),
          DTOutput("row_added_tbl"),
          DTOutput("row_removed_tbl"),
          DTOutput("value_changes_tbl")
        )
      )
    )
  )
)

# ---------------------------- Server --------------------------------

server <- function(input, output, session) {
  
  # ---------- Tab 1: Verify with Receipt ----------
  
  rec <- reactive({
    req(input$receipt)
    fromJSON(input$receipt$datapath, simplifyVector = TRUE)
  })
  
  output$receipt_table <- renderDT({
    r <- rec()
    meta <- data.frame(
      field = c("timestamp_utc", "url", "doi", "http_status", "content_type",
                "content_length", "etag", "last_modified",
                "local_path", "size_bytes", "sha256", "extension"),
      value = c(
        r$timestamp_utc %||% NA,
        r$source$url %||% NA,
        r$source$doi %||% NA,
        r$source$http_status %||% NA,
        r$source$content_type %||% NA,
        r$source$content_length %||% NA,
        r$source$etag %||% NA,
        r$source$last_modified %||% NA,
        r$file$path %||% NA,
        r$file$size_bytes %||% NA,
        r$file$sha256 %||% NA,
        r$file$extension %||% NA
      ),
      stringsAsFactors = FALSE
    )
    datatable(meta, rownames = FALSE, options = list(dom = "t", pageLength = nrow(meta)))
  })
  
  output$receipt_note <- renderText({
    r <- req(rec())
    if (isTruthy(r$file$path) && is.null(input$datafile)) {
      paste0("Note: receipt references local path:\n  ", r$file$path,
             "\nUpload that file above to run local verification here.")
    } else ""
  })
  
  output$receipt_json <- renderText({
    req(input$receipt)
    paste(readLines(input$receipt$datapath, warn = FALSE), collapse = "\n")
  })
  
  observeEvent(input$check_local, {
    output$local_result <- renderText({
      r <- rec()
      if (is.null(input$datafile)) {
        return("Local verification: no data file uploaded.")
      }
      got_sha <- tolower(hash_file(input$datafile$datapath))
      exp_sha <- tolower(r$file$sha256 %||% "")
      if (!nzchar(exp_sha)) return("Local verification: receipt has no sha256 field.")
      if (identical(got_sha, exp_sha)) {
        paste0("Local verification: match\n  expected sha256: ", exp_sha, "\n  got      sha256: ", got_sha)
      } else {
        paste0("Local verification: MISMATCH\n  expected sha256: ", exp_sha, "\n  got      sha256: ", got_sha)
      }
    })
  })
  
  observeEvent(input$check_remote, {
    output$remote_result <- renderText({
      r <- rec()
      url <- r$source$url %||% ""
      if (!nzchar(url)) return("Remote verification: receipt has no URL.")
      tmp <- fs::file_temp(ext = r$file$extension %||% "")
      on.exit({ if (file_exists(tmp)) file_delete(tmp) }, add = TRUE)
      resp <- try(request(url) |> req_perform(), silent = TRUE)
      if (inherits(resp, "try-error")) return(paste("Remote verification: download failed for URL:", url))
      writeBin(resp_body_raw(resp), tmp)
      got_sha <- tolower(hash_file(tmp))
      exp_sha <- tolower(r$file$sha256 %||% "")
      if (!nzchar(exp_sha)) return("Remote verification: receipt has no sha256 field.")
      if (identical(got_sha, exp_sha)) {
        paste0("Remote verification: match\n  expected sha256: ", exp_sha, "\n  got      sha256: ", got_sha)
      } else {
        paste0("Remote verification: MISMATCH\n  expected sha256: ", exp_sha, "\n  got      sha256: ", got_sha)
      }
    })
  })
  
  # ---------- Tab 2: Compare Two Files ----------
  
  observeEvent(input$compare, {
    output$hash_compare <- renderText({
      req(input$fileA, input$fileB)
      shaA <- tolower(hash_file(input$fileA$datapath))
      shaB <- tolower(hash_file(input$fileB$datapath))
      if (identical(shaA, shaB)) {
        paste0("Files are byte-identical.\nSHA-256: ", shaA)
      } else {
        paste0("Files differ.\nFile A SHA-256: ", shaA, "\nFile B SHA-256: ", shaB)
      }
    })
    
    # Try to read as tabular data and diff
    dfA <- try(read_tabular(req(input$fileA)$datapath), silent = TRUE)
    dfB <- try(read_tabular(req(input$fileB)$datapath), silent = TRUE)
    
    if (inherits(dfA, "try-error") || inherits(dfB, "try-error")) {
      output$schema_compare   <- renderText({"Schema diff: skipped (not CSV/TSV or unreadable)."})
      output$rowcell_summary  <- renderText({"Row/cell diff: skipped (not CSV/TSV or unreadable)."})
      output$row_added_tbl    <- renderDT({datatable(data.frame())})
      output$row_removed_tbl  <- renderDT({datatable(data.frame())})
      output$value_changes_tbl<- renderDT({datatable(data.frame())})
      return()
    }
    
    # Schema diff
    sd <- schema_diff(dfA, dfB)
    output$schema_compare <- renderText({
      paste0(
        "Columns added:  ", if (length(sd$added)) paste(sd$added, collapse = ", ") else "(none)", "\n",
        "Columns removed:", if (length(sd$removed)) paste(sd$removed, collapse = ", ") else "(none)"
      )
    })
    
    # Row/cell diff only if keys and value_col provided
    keys_str <- trimws(input$keys %||% "")
    value_col <- trimws(input$value_col %||% "")
    if (!nzchar(keys_str) || !nzchar(value_col)) {
      output$rowcell_summary   <- renderText({"Row/cell diff: provide key columns and value column to compare."})
      output$row_added_tbl     <- renderDT({datatable(data.frame())})
      output$row_removed_tbl   <- renderDT({datatable(data.frame())})
      output$value_changes_tbl <- renderDT({datatable(data.frame())})
      return()
    }
    
    keys <- strsplit(keys_str, "\\s*,\\s*")[[1]]
    if (!all(keys %in% names(dfA)) || !all(keys %in% names(dfB)) || !(value_col %in% names(dfA)) || !(value_col %in% names(dfB))) {
      output$rowcell_summary   <- renderText({"Row/cell diff: keys/value column not found in both files."})
      output$row_added_tbl     <- renderDT({datatable(data.frame())})
      output$row_removed_tbl   <- renderDT({datatable(data.frame())})
      output$value_changes_tbl <- renderDT({datatable(data.frame())})
      return()
    }
    
    rd <- row_cell_diff(dfA, dfB, keys = keys, value_col = value_col)
    output$rowcell_summary <- renderText({
      paste0("Rows added: ", rd$summary$rows_added,
             " | Rows removed: ", rd$summary$rows_removed,
             " | Values changed: ", rd$summary$values_changed)
    })
    output$row_added_tbl     <- renderDT(datatable(head(rd$added_rows, 100), options = list(pageLength = 10)))
    output$row_removed_tbl   <- renderDT(datatable(head(rd$removed_rows, 100), options = list(pageLength = 10)))
    output$value_changes_tbl <- renderDT(datatable(head(rd$value_changes, 100), options = list(pageLength = 10)))
  })
}

shinyApp(ui, server)
