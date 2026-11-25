# tests/testthat/test-catalog.R

test_that("upsert_lake_catalog inserts and updates idempotently", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "CREATE SCHEMA qc_lake;")
  DBI::dbExecute(
    con,
    "
    CREATE TABLE qc_lake.lake_catalog (
      dataset_name VARCHAR,
      source_system VARCHAR,
      format_type VARCHAR,
      location_ref VARCHAR,
      raw_path VARCHAR,
      crs_epsg INTEGER,
      notes VARCHAR,
      last_ingested_at TIMESTAMP
    );
  "
  )

  upsert_lake_catalog(
    con = con,
    dataset_name = "qc_regions",
    source_system = "Données Québec",
    format_type = "ducklake_table",
    location_ref = "qc_lake.qc_regions",
    crs_epsg = 32198L,
    notes = "Initial notes"
  )

  upsert_lake_catalog(
    con = con,
    dataset_name = "qc_regions",
    source_system = "Données Québec",
    format_type = "ducklake_table",
    location_ref = "qc_lake.qc_regions",
    crs_epsg = 32198L,
    notes = "Updated notes"
  )

  rows <- DBI::dbGetQuery(
    con,
    "SELECT * FROM qc_lake.lake_catalog WHERE dataset_name = 'qc_regions';"
  )
  expect_equal(nrow(rows), 1L)
  expect_equal(rows$notes, "Updated notes")
})

test_that("read_lake_catalog filters by dataset_name when requested", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(
    con,
    "
    CREATE TABLE lake_catalog (dataset_name VARCHAR, location_ref VARCHAR);
  "
  )
  DBI::dbExecute(
    con,
    "
    INSERT INTO lake_catalog VALUES
      ('b_table', 'qc_lake.b_table'),
      ('a_table', 'qc_lake.a_table');
  "
  )

  all_rows <- read_lake_catalog(con)
  expect_equal(all_rows$dataset_name, c("a_table", "b_table"))

  one_row <- read_lake_catalog(con, dataset_name = "b_table")
  expect_equal(nrow(one_row), 1L)
  expect_equal(one_row$location_ref, "qc_lake.b_table")
})

test_that("read_lookup_csv preserves leading zeros and warns on semicolon or non-UTF-8", {
  skip_if_not_installed("readr")

  lookups_dir <- tempfile("lookups_")
  dir.create(lookups_dir, recursive = TRUE)
  on.exit(unlink(lookups_dir, recursive = TRUE), add = TRUE)

  # Standard UTF-8 (including accented) comma lookup with leading zero
  writeLines(
    enc2utf8(c("code,region", "01,Bas-Saint-Laurent", "06,Montréal")),
    file.path(lookups_dir, "clean_reg.csv"),
    useBytes = TRUE
  )
  df_clean <- expect_warning(
    read_lookup_csv("clean_reg", lookups_dir = lookups_dir),
    NA
  )
  expect_type(df_clean$code, "character")
  expect_equal(df_clean$code, c("01", "06"))
  expect_equal(df_clean$region[[2L]], "Montréal")

  # Semicolon-delimited lookup should warn about delimiter
  writeLines(
    c("code;region", "02;Saguenay"),
    file.path(lookups_dir, "semi_reg.csv")
  )
  expect_warning(
    df_semi <- read_lookup_csv("semi_reg.csv", lookups_dir = lookups_dir),
    "delimiter"
  )
  expect_equal(df_semi$code, "02")
})

test_that("normalize_lookup_csv writes standard UTF-8 comma output", {
  skip_if_not_installed("readr")

  lookups_dir <- tempfile("lookups_norm_")
  dir.create(lookups_dir, recursive = TRUE)
  on.exit(unlink(lookups_dir, recursive = TRUE), add = TRUE)

  writeLines(
    c("code;region", "04;Mauricie"),
    file.path(lookups_dir, "raw_semi.csv")
  )

  out_path <- suppressWarnings(
    normalize_lookup_csv(
      "raw_semi",
      lookups_dir = lookups_dir,
      overwrite = FALSE
    )
  )
  expect_true(file.exists(out_path))

  lines <- readLines(out_path, encoding = "UTF-8")
  expect_equal(lines, c("code,region", "04,Mauricie"))
})
