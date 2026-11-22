#' Expire Old DuckLake Snapshots
#'
#' Expires specified snapshot versions in a connected DuckLake catalog.
#'
#' @param con A valid `DBI` connection to DuckDB / DuckLake.
#' @param lake_alias Single non-empty string naming the attached DuckLake
#'   catalog (e.g., `"qc_lake"`).
#' @param versions Integer vector of `snapshot_id` values to expire.
#'
#' @return Invisibly returns `TRUE`.
#' @export
#' @md
expire_lake_snapshots <- function(con, lake_alias, versions) {
  if (!inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
    cli::cli_abort("{.arg con} must be a valid DBI connection.")
  }
  assert_non_empty_string(lake_alias, "lake_alias")

  if (!is.numeric(versions) || length(versions) < 1L || any(is.na(versions))) {
    cli::cli_abort(
      "{.arg versions} must be a non-empty vector of snapshot IDs."
    )
  }

  alias_sql <- DBI::dbQuoteString(con, lake_alias)
  versions_sql <- paste(as.integer(versions), collapse = ", ")

  DBI::dbExecute(
    con,
    sprintf(
      "CALL ducklake_expire_snapshots(%s, versions => [%s]);",
      alias_sql,
      versions_sql
    )
  )

  invisible(TRUE)
}

#' Clean Up Unreferenced DuckLake Data Files
#'
#' Previews or removes Parquet files in `data/lake/` that are no longer
#' referenced by any active snapshot.
#'
#' @param con A valid `DBI` connection to DuckDB / DuckLake.
#' @param lake_alias Single non-empty string naming the attached DuckLake
#'   catalog (e.g., `"qc_lake"`).
#' @param cleanup_all Logical flag passed to `ducklake_cleanup_old_files`.
#' @param dry_run Logical flag; when `TRUE` (default), returns candidate file
#'   paths without deleting anything.
#'
#' @return A `data.frame` of candidate files when `dry_run = TRUE`, or `TRUE`
#'   invisibly when executed for deletion.
#' @export
#' @md
cleanup_lake_files <- function(
  con,
  lake_alias,
  cleanup_all = TRUE,
  dry_run = TRUE
) {
  if (!inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
    cli::cli_abort("{.arg con} must be a valid DBI connection.")
  }
  assert_non_empty_string(lake_alias, "lake_alias")
  assert_flag(cleanup_all, "cleanup_all")
  assert_flag(dry_run, "dry_run")

  alias_sql <- DBI::dbQuoteString(con, lake_alias)
  all_sql <- if (isTRUE(cleanup_all)) "TRUE" else "FALSE"

  if (isTRUE(dry_run)) {
    return(
      DBI::dbGetQuery(
        con,
        sprintf(
          "CALL ducklake_cleanup_old_files(%s, cleanup_all = %s, dry_run = TRUE);",
          alias_sql,
          all_sql
        )
      )
    )
  }

  DBI::dbExecute(
    con,
    sprintf(
      "CALL ducklake_cleanup_old_files(%s, cleanup_all = %s);",
      alias_sql,
      all_sql
    )
  )

  invisible(TRUE)
}
