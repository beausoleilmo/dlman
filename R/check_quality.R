#' Run post-ingestion data quality checks on a DuckLake instance
#'
#' Validates that all managed tables listed in `lake_catalog` exist and contain
#' at least one row, verifies that referenced raster files exist on disk, and
#' checks for null geometries in specified WKB geometry columns.
#'
#' @param con An active `DBIConnection` attached to the DuckLake catalog (such
#'   as returned by [connect_lake()]).
#' @param lake_dir Root directory of the data lake, used to resolve relative
#'   raster paths in `lake_catalog`.
#' @param geom_checks Named character vector mapping table names to their WKB
#'   geometry column names to check for `NULL` values (e.g.,
#'   `c(admin_municipalites = "geom_wkb")`). Set to `NULL` to skip.
#'
#' @return Invisibly returns `TRUE` if all quality assertions pass. Throws an
#'   informative error listing all failed checks otherwise.
#' @importFrom DBI dbIsValid dbGetQuery dbExistsTable
#' @examples
#' \dontrun{
#' con <- connect_lake("path/to/qc-datalake", read_only = TRUE)
#' on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
#' check_quality(con, lake_dir = "path/to/qc-datalake")
#' }
#' @export
check_quality <- function(
  con,
  lake_dir = ".",
  geom_checks = c(admin_municipalites = "geom_wkb")
) {
  if (!DBI::dbIsValid(con)) {
    stop("`con` is not a valid DBI connection.", call. = FALSE)
  }

  errors <- character()

  # 1. Check managed DuckLake tables have > 0 rows
  tables_df <- DBI::dbGetQuery(
    con,
    "SELECT dataset_name, location_ref 
    FROM lake_catalog 
    WHERE format_type = 'ducklake_table';"
  )

  for (i in seq_len(nrow(tables_df))) {
    ds_name <- tables_df$dataset_name[i]
    tbl_name <- sub("^[^.]+\\.", "", tables_df$location_ref[i])

    if (!DBI::dbExistsTable(con, tbl_name)) {
      errors <- c(
        errors,
        sprintf("[%s] Registered table '%s' does not exist.", ds_name, tbl_name)
      )
      next
    }

    n_rows <- DBI::dbGetQuery(
      con,
      sprintf("SELECT COUNT(*) AS n FROM %s;", tbl_name)
    )$n
    if (is.na(n_rows) || n_rows == 0L) {
      errors <- c(
        errors,
        sprintf("[%s] Table '%s' is empty (0 rows).", ds_name, tbl_name)
      )
    }
  }

  # 2. Check registered raster assets exist on disk
  rasters_df <- DBI::dbGetQuery(
    con,
    "SELECT dataset_name, location_ref 
    FROM lake_catalog 
    WHERE format_type = 'raster_tif';"
  )

  for (i in seq_len(nrow(rasters_df))) {
    ds_name <- rasters_df$dataset_name[i]
    rel_path <- rasters_df$location_ref[i]
    full_path <- file.path(lake_dir, rel_path)

    if (!file.exists(full_path) && !dir.exists(full_path)) {
      errors <- c(
        errors,
        sprintf("[%s] Raster path not found: '%s'", ds_name, full_path)
      )
    }
  }

  # 3. Check vector tables for null geometries
  if (!is.null(geom_checks) && length(geom_checks) > 0L) {
    for (tbl_name in names(geom_checks)) {
      if (!DBI::dbExistsTable(con, tbl_name)) {
        next
      }
      geom_col <- geom_checks[[tbl_name]]
      n_null <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT COUNT(*) AS n 
          FROM %s 
          WHERE %s IS NULL;",
          tbl_name,
          geom_col
        )
      )$n
      if (!is.na(n_null) && n_null > 0L) {
        errors <- c(
          errors,
          sprintf(
            "[%s] Found %d NULL geometries in column '%s'.",
            tbl_name,
            n_null,
            geom_col
          )
        )
      }
    }
  }

  if (length(errors) > 0L) {
    stop(
      "Data quality checks failed:\n",
      paste0("  - ", errors, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
