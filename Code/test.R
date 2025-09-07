# --------------------------------------------------------------------
# Reproducibility Receipts — Demo Script
#   - NUTS 2021 vs 2024 (different vintages → different hashes)
#   - Eurostat bulk GDP snapshot (timestamped) + ledger + signing
# Requirements: reprorec.R (the consolidated version you sourced earlier)
# --------------------------------------------------------------------

# 0) Setup ------------------------------------------------------------

setwd("C:/Users/benva/OneDrive/Documents/reprorec/Code")
source("reprorec.R")  # provides rr_fetch, rr_verify, rr_snapshot, rr_log, rr_sign, rr_log_publish, rr_log_merkle, etc.

# 1) NUTS demo: create receipts and verify ---------------------------

nuts21 <- rr_fetch(
  url  = "https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2021-01m.geojson.zip",
  dest = "data/nuts_2021_01m.geojson.zip"
)
rr_verify(nuts21$receipt, mode = "local")
rr_verify(nuts21$receipt, mode = "remote")

nuts24 <- rr_fetch(
  url  = "https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2024-01m.geojson.zip",
  dest = "data/nuts_2024_01m.geojson.zip"
)
rr_verify(nuts24$receipt, mode = "local")
rr_verify(nuts24$receipt, mode = "remote")

# Helper: compare two receipts (hash equality)
rr_compare_receipts <- function(r1, r2) {
  a <- jsonlite::read_json(r1, simplifyVector = TRUE)
  b <- jsonlite::read_json(r2, simplifyVector = TRUE)
  data.frame(
    fileA = basename(a$file$path),
    fileB = basename(b$file$path),
    shaA  = a$file$sha256,
    shaB  = b$file$sha256,
    same  = identical(tolower(a$file$sha256), tolower(b$file$sha256)),
    stringsAsFactors = FALSE
  )
}
rr_compare_receipts(nuts21$receipt, nuts24$receipt)

# Helper: tidy receipt summary (for README/tables)
pretty_receipt <- function(rp) {
  rec <- jsonlite::read_json(rp, simplifyVector = TRUE)
  data.frame(
    file       = basename(rec$file$path),
    url        = rec$source$url,
    size_bytes = rec$file$size_bytes,
    sha256     = rec$file$sha256,
    timestamp  = rec$timestamp_utc,
    stringsAsFactors = FALSE
  )
}
rbind(pretty_receipt(nuts21$receipt), pretty_receipt(nuts24$receipt))

# 2) Eurostat bulk GDP snapshot (timestamped) ------------------------

# Use the Files API for the whole table (twice-daily updates at the same URL)
bulk_url <- "https://ec.europa.eu/eurostat/api/dissemination/files/data/nama_10_gdp.tsv.gz"

# Save with a timestamped filename so you can keep multiple daily versions
bulk_dest <- sprintf("data/nama_10_gdp_%s.tsv.gz", format(Sys.time(), "%Y-%m-%d_%H%M"))
snap <- rr_fetch(bulk_url, dest = bulk_dest)

# Verify against current remote (should be unchanged right after fetch)
rr_verify(snap$receipt, mode = "remote")

# Log this snapshot to the simple ledger
rr_log(snap$receipt)

# Inspect the ledger (sorted by time)
ledger <- readr::read_csv("receipts/_ledger.csv", show_col_types = FALSE)
ledger <- ledger[order(ledger$time), ]
print(ledger, n = min(10L, nrow(ledger)))

# 3) Optional: sign the new receipt and publish to a public log ------

# Sign with your GPG key (adjust email if needed)
sig_path <- rr_sign(snap$receipt, gpg_key = "jacqueline-seufert@web.de")

# Append a row to the public log (CSV); commit this file to a repo you share
rr_log_publish(snap$receipt, sig_path)

# Compute a Merkle root over the public log (optional; can be timestamped later)
rr_log_merkle()  # writes public-log/merkle.json

# 4) Notes -----------------------------------------------------------
# - To track changes over time, run the “Eurostat bulk GDP snapshot” block
#   again after each Eurostat update window. Each run creates a new file,
#   a new receipt, and a new ledger row.
# - To compare any two versions programmatically, call rr_compare_receipts()
#   on their two receipt paths.


# Example: GDP bulk file snapshots (same URL, code = "nama_10_gdp")
snap1 <- rr_snapshot("https://ec.europa.eu/eurostat/api/dissemination/files/data/nama_10_gdp.tsv.gz", code = "nama_10_gdp")
# ... later ...
snap2 <- rr_snapshot("https://ec.europa.eu/eurostat/api/dissemination/files/data/nama_10_gdp.tsv.gz", code = "nama_10_gdp")
# ... repeat ...
diff_last <- rr_diff_rows_latest(
  code = "nama_10_gdp",
  keys = c("geo","s_adj","na_item","unit","time"),
  value_col = "value"
)
diff_last$summary        # compact numbers
diff_last$value_changes  # detailed cell changes
chg <- rr_changelog_latest(
  code = "nama_10_gdp",
  keys = c("geo","s_adj","na_item","unit","time"),
  value_col = "value"
)
cat(chg$text)

tri <- rr_revision_triangle(
  code = "nama_10_gdp",
  keys_filter = list(geo = "BE", na_item = "B1GQ", unit = "CP_MEUR", s_adj = "NSA"),
  time_col = "time",
  value_col = "value",
  n_vintages = 8
)

# View the triangle
tri$triangle_wide         # vintages (rows) × time periods (cols)
tri$revisions_vs_first    # differences vs first vintage
tri$revisions_vs_prev     # differences vs previous vintage


