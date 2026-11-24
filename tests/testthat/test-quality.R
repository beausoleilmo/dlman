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
      location_ref VARCHAR
    );
  "
  )
  DBI::dbExecute(
    con,
    "
    INSERT INTO lake_catalog VALUES ('admin_municipalites', 'ducklake_table', 'admin_municipalites');
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
