#' Validate a datasets catalog YAML file
#'
#' Checks that a YAML catalog contains a non-empty `datasets` mapping, that
#' every dataset entry defines all required fields with non-empty values, and
#' that controlled fields match allowed keyword values.
#'
#' @param catalog_path Path to the catalog YAML file.
#' @param required_fields Character vector of required field names per dataset.
#' @param allowed_values Named list mapping field names to character vectors of
#'   permitted values (e.g., `list(license = c("CC-BY 4.0", "CC0 1.0"))`).
#'   Pass `NULL` to skip controlled-vocabulary checks.
#'
#' @return Invisibly returns `TRUE` if validation succeeds. Throws an error if
#'   the file is missing, malformed, or violates field rules.
#' @importFrom yaml read_yaml
#' @examples
#' \dontrun{
#' check_catalog("catalog/datasets.yml")
#'
#' check_catalog(
#'   catalog_path = "catalog/datasets.yml",
#'   allowed_values = list(license = c("CC-BY 4.0", "CC0 1.0"))
#' )
#' }
#' @export
check_catalog <- function(
  catalog_path = "catalog/datasets.yml",
  required_fields = c("title", "source", "raw_path", "crs", "license", "notes"),
  allowed_values = list(
    license = c("CC-BY 4.0", "CC0 1.0", "Open Government Licence - Canada")
  )
) {
  if (
    !is.character(catalog_path) ||
      length(catalog_path) != 1L ||
      !nzchar(catalog_path)
  ) {
    stop(
      "`catalog_path` must be a single non-empty character string.",
      call. = FALSE
    )
  }
  if (!is.character(required_fields) || length(required_fields) == 0L) {
    stop(
      "`required_fields` must be a non-empty character vector.",
      call. = FALSE
    )
  }
  if (!file.exists(catalog_path)) {
    stop("Catalog file not found: ", catalog_path, call. = FALSE)
  }

  spec <- yaml::read_yaml(catalog_path)
  datasets <- spec$datasets

  if (is.null(datasets) || length(datasets) == 0L) {
    stop("No entries found under 'datasets:' in ", catalog_path, call. = FALSE)
  }

  ds_names <- names(datasets)
  if (is.null(ds_names) || any(!nzchar(trimws(ds_names)))) {
    stop(
      "Every dataset entry under 'datasets:' must have a non-empty name.",
      call. = FALSE
    )
  }

  errors <- character()

  for (ds_name in ds_names) {
    entry <- datasets[[ds_name]]

    if (!is.list(entry)) {
      errors <- c(
        errors,
        sprintf("[%s] Entry must be a mapping/list.", ds_name)
      )
      next
    }

    for (field in required_fields) {
      val <- entry[[field]]
      is_empty <- is.null(val) ||
        (is.character(val) && (length(val) == 0L || all(!nzchar(trimws(val)))))
      if (is_empty) {
        errors <- c(
          errors,
          sprintf("[%s] Missing or empty required field: '%s'", ds_name, field)
        )
      }
    }

    if (!is.null(allowed_values)) {
      for (field in names(allowed_values)) {
        val <- entry[[field]]
        if (!is.null(val) && !(val %in% allowed_values[[field]])) {
          errors <- c(
            errors,
            sprintf(
              "[%s] Invalid '%s': '%s'. Allowed: %s",
              ds_name,
              field,
              as.character(val),
              paste(allowed_values[[field]], collapse = ", ")
            )
          )
        }
      }
    }
  }

  if (length(errors) > 0L) {
    stop(
      "Catalog validation failed:\n",
      paste0("  - ", errors, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


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
