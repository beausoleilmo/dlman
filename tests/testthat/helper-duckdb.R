# tests/testthat/helper-duckdb.R

local_duckdb_con <- function(
  dbdir = ":memory:",
  shared_home = FALSE,
  env = parent.frame()
) {
  if (isTRUE(shared_home)) {
    drv <- duckdb::duckdb(dbdir = dbdir)
  } else {
    tmp_home <- withr::local_tempdir(
      pattern = "duckdb_home_",
      .local_envir = env
    )
    drv <- duckdb::duckdb(dbdir = dbdir, shared_home = FALSE)
  }

  con <- DBI::dbConnect(drv)
  withr::defer(
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE),
    envir = env
  )
  con
}
