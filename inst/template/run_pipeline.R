#!/usr/bin/env Rscript
# scripts/R/run_pipeline.R — End-to-end local DataOps pipeline runner

# Load package functions (use library(yourpkg) once installed)
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  source("R/connect_lake.R")
  source("R/check_catalog.R")
  source("R/check_quality.R")
}

lake_dir <- "."

message("1/4 Validating catalog/datasets.yml...")
check_catalog(file.path(lake_dir, "catalog/datasets.yml"))

message(
  "2/4 Connecting to DuckLake (read-write) and running ingestion scripts..."
)
con <- connect_lake(lake_dir = lake_dir, read_only = FALSE)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

sql_scripts <- sort(list.files(
  file.path(lake_dir, "scripts/sql"),
  pattern = "^ingest_.*\\.sql$",
  full.names = TRUE
))

if (length(sql_scripts) == 0L) {
  stop(
    "No ingestion scripts matching 'ingest_*.sql' found in scripts/sql/.",
    call. = FALSE
  )
}

for (sql_file in sql_scripts) {
  message("  -> Executing ", basename(sql_file))
  sql_text <- paste(readLines(sql_file, warn = FALSE), collapse = "\n")
  DBI::dbExecute(con, sql_text)
}

message("3/4 Running post-ingestion data quality checks...")
check_quality(con, lake_dir = lake_dir)

message("4/4 Pipeline complete. Latest DuckLake snapshot:")
snap <- DBI::dbGetQuery(
  con,
  "SELECT * FROM ducklake_snapshots('qc_lake') ORDER BY snapshot_id DESC LIMIT 1;"
)
print(snap)
