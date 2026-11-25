# tests/testthat/test-quality.R
test_that("check_quality passes for non-empty table and non-null geometry", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(
    con,
    "
    CREATE TABLE lake_catalog (
      dataset_name VARCHAR,
      format_type VARCHAR,
      location_ref VARCHAR, 
      raw_path VARCHAR
    );
  "
  )
  DBI::dbExecute(
    con,
    "
    INSERT INTO lake_catalog 
    VALUES ('admin_municipalites', 'ducklake_table', 'admin_municipalites', '.');
  "
  )
  DBI::dbExecute(
    con,
    "
    CREATE TABLE admin_municipalites (id INTEGER, geom_wkb BLOB);
  "
  )
  DBI::dbExecute(
    con,
    "
    INSERT INTO admin_municipalites VALUES (1, '\\x010100000000000000000000000000000000000000'::BLOB);
  "
  )

  expect_true(check_quality(
    con,
    geom_checks = c(admin_municipalites = "geom_wkb")
  ))
})


test_that("check_quality validates COG raster entries when present and errors when missing", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("withr")

  tmp_home <- withr::local_tempdir(pattern = "dlman_q_home_")
  tmp_lake <- withr::local_tempdir(pattern = "dlman_q_lake_")
  cog_dir <- file.path(tmp_lake, "data", "cog")
  dir.create(cog_dir, recursive = TRUE)

  cog_file <- file.path(cog_dir, "qc_lulc_2025.tif")
  writeBin(as.raw(c(0x49, 0x49, 0x2a, 0x00, 0x01, 0x02, 0x03, 0x04)), cog_file)

  con <- DBI::dbConnect(duckdb::duckdb(home = tmp_home))
  withr::defer(
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE lake_catalog (
      dataset_name VARCHAR,
      format_type VARCHAR,
      location_ref VARCHAR,
      raw_path VARCHAR
    );
    "
  )

  DBI::dbExecute(
    con,
    sprintf(
      "
      INSERT INTO lake_catalog (dataset_name, format_type, location_ref)
      VALUES ('qc_lulc_2025', 'cog', %s);
      ",
      DBI::dbQuoteString(con, cog_file)
    )
  )

  # Passes when the COG file exists and is non-empty
  expect_true(
    check_quality(con = con, lake_dir = tmp_lake, geom_checks = NULL)
  )

  # Errors when the COG file is missing
  unlink(cog_file)
  expect_error(
    check_quality(con = con, lake_dir = tmp_lake, geom_checks = NULL),
    "Raster path not found"
  )
})
