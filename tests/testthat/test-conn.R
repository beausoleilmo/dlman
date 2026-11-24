# tests/testthat/test-conn.R

test_that("connect_lake works with an isolated temporary DuckDB home", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("withr")

  tmp_home <- withr::local_tempdir(pattern = "dlman_home_")
  lake_dir <- withr::local_tempdir(pattern = "dlman_lake_")
  dir.create(file.path(lake_dir, "data", "lake"), recursive = TRUE)

  drv <- duckdb::duckdb(home = tmp_home)
  con <- tryCatch(
    connect_lake(
      lake_dir = lake_dir,
      read_only = FALSE,
      drv = drv,
      dbdir = ":memory:",
      extensions = c("ducklake", "spatial"),
      community_extensions = character()
    ),
    error = function(e) {
      skip(paste("DuckLake extension unavailable in test env:", e$message))
    }
  )
  withr::defer(
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
  )

  expect_true(DBI::dbIsValid(con))
  expect_true(dir.exists(tmp_home))
})


test_that("connect_lake validates arguments and missing read-only catalog", {
  expect_error(
    connect_lake(lake_dir = ""),
    "`lake_dir` must be a single non-empty path"
  )

  expect_error(
    connect_lake(lake_dir = file.path(tempdir(), "does_not_exist_12345")),
    "Lake directory does not exist"
  )

  empty_dir <- tempfile("dlman_empty_")
  dir.create(empty_dir, recursive = TRUE)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

  expect_error(
    connect_lake(lake_dir = empty_dir, read_only = TRUE),
    "Catalog file not found for read-only connection"
  )
})

test_that("connect_lake creates a DuckLake catalog, writes data, and reads back in READ_ONLY mode", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  lake_dir <- tempfile("dlman_lake_")
  dir.create(file.path(lake_dir, "data", "lake"), recursive = TRUE)
  on.exit(unlink(lake_dir, recursive = TRUE), add = TRUE)

  # 1. Open read-write connection to initialize catalog and create a table
  con_rw <- tryCatch(
    connect_lake(
      lake_dir = lake_dir,
      read_only = FALSE,
      extensions = c("ducklake", "spatial"),
      community_extensions = character()
    ),
    error = function(e) {
      skip(paste("DuckLake extension unavailable in test env:", e$message))
    }
  )
  on.exit(
    if (DBI::dbIsValid(con_rw)) DBI::dbDisconnect(con_rw, shutdown = TRUE),
    add = TRUE
  )

  DBI::dbExecute(
    con_rw,
    "
    CREATE TABLE IF NOT EXISTS sample_items (
      id INTEGER,
      label VARCHAR
    );
  "
  )
  DBI::dbExecute(
    con_rw,
    "INSERT INTO sample_items VALUES (1, 'Montreal'), (2, 'Quebec');"
  )

  # Close read-write connection before opening read-only test connection
  DBI::dbDisconnect(con_rw, shutdown = TRUE)
  expect_true(file.exists(file.path(lake_dir, "catalog.ducklake")))

  # 2. Reconnect in READ_ONLY mode and verify data + write rejection
  con_ro <- connect_lake(
    lake_dir = lake_dir,
    read_only = TRUE,
    extensions = c("ducklake", "spatial"),
    community_extensions = character()
  )
  on.exit(
    if (DBI::dbIsValid(con_ro)) DBI::dbDisconnect(con_ro, shutdown = TRUE),
    add = TRUE
  )

  res <- DBI::dbGetQuery(
    con_ro,
    "SELECT id, label FROM sample_items ORDER BY id;"
  )
  expect_equal(nrow(res), 2L)
  expect_equal(res$label, c("Montreal", "Quebec"))

  # Verify READ_ONLY blocks writes
  expect_error(
    DBI::dbExecute(con_ro, "INSERT INTO sample_items VALUES (3, 'Laval');")
  )
})

test_that("load_duckdb_extensions sets geometry_always_xy when spatial is loaded", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )

  tryCatch(
    load_duckdb_extensions(
      con,
      extensions = "spatial",
      community_extensions = character()
    ),
    error = function(e) {
      skip(paste("spatial extension unavailable in test env:", e$message))
    }
  )

  xy_setting <- DBI::dbGetQuery(
    con,
    "SELECT current_setting('geometry_always_xy') AS xy;"
  )$xy

  expect_true(as.logical(xy_setting))
})
