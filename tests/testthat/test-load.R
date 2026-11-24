# tests/testthat/test-load.R

test_that("write_lake_table validates inputs", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_error(
    write_lake_table(con = con, table_name = "", parquet_path = "dummy.parquet")
  )
  expect_error(
    write_lake_table(
      con = con,
      table_name = "qc_regions",
      parquet_path = file.path(tempdir(), "missing_file_999.parquet")
    )
  )
})

test_that("write_lake_table writes and replaces table rows from Parquet", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("arrow")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  pq1 <- tempfile(fileext = ".parquet")
  pq2 <- tempfile(fileext = ".parquet")
  on.exit(unlink(c(pq1, pq2)), add = TRUE)

  arrow::write_parquet(
    data.frame(code = c("01", "02"), name = c("Bas-Saint-Laurent", "Saguenay")),
    pq1
  )
  arrow::write_parquet(
    data.frame(code = c("03"), name = c("Capitale-Nationale")),
    pq2
  )

  write_lake_table(con = con, table_name = "qc_regions", parquet_path = pq1)
  res1 <- DBI::dbGetQuery(con, "SELECT * FROM qc_regions ORDER BY code;")
  expect_equal(nrow(res1), 2L)
  expect_equal(res1$code, c("01", "02"))

  # Verify full replacement behavior
  write_lake_table(con = con, table_name = "qc_regions", parquet_path = pq2)
  res2 <- DBI::dbGetQuery(con, "SELECT * FROM qc_regions;")
  expect_equal(nrow(res2), 1L)
  expect_equal(res2$code, "03")
})
