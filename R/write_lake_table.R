#' Materialize a Parquet File into a Lake Table
#'
#' Creates or replaces a target table in the connected DuckDB / DuckLake
#' session from a local Parquet file.
#'
#' @param con A valid `DBI` connection to DuckDB / DuckLake.
#' @param table_name Single non-empty character string naming the target table
#'   (e.g., `"qc_regions"` or `"qc_lake.qc_regions"`).
#' @param parquet_path Single non-empty character string pointing to an existing
#'   `.parquet` file.
#'
#' @return Invisibly returns `TRUE`.
#' @export
#' @md
write_lake_table <- function(con, table_name, parquet_path) {
  if (!inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
    cli::cli_abort("{.arg con} must be a valid DBI connection.")
  }
  assert_non_empty_string(table_name, "table_name")
  assert_non_empty_string(parquet_path, "parquet_path")

  if (!file.exists(parquet_path)) {
    cli::cli_abort("Parquet file does not exist at {.path {parquet_path}}.")
  }

  norm_path <- normalizePath(parquet_path, winslash = "/", mustWork = TRUE)
  table_sql <- DBI::dbQuoteIdentifier(con, table_name)
  path_sql <- DBI::dbQuoteString(con, norm_path)

  DBI::dbExecute(
    con,
    sprintf(
      "
      CREATE OR REPLACE TABLE %s AS
      SELECT *
      FROM read_parquet(%s);
      ",
      table_sql,
      path_sql
    )
  )

  invisible(TRUE)
}
