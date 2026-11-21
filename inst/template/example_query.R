# Starter template for querying a local lake using dlman
library(dlman)
library(DBI)
library(sf)
library(terra)

lake_dir <- "/path/to/qc-datalake"
con <- connect_lake(lake_dir = lake_dir, read_only = TRUE)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

print(dbGetQuery(
  con,
  "SELECT dataset_name, format_type, crs_epsg, last_ingested_at FROM lake_catalog;"
))
