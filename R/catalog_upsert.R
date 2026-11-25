#' Upsert a Dataset Entry into `lake_catalog`
#'
#' Inserts or replaces a dataset metadata row in `lake_catalog` within a
#' transaction so repeated ingestion runs update the entry in place instead of
#' appending duplicate rows.
#'
#' @param con A `DBI` connection to DuckDB / DuckLake.
#' @param dataset_name Single non-empty character string identifying the dataset.
#' @param source_system Single non-empty character string for the upstream source
#'   (e.g., `"Données Québec"`).
#' @param format_type Single non-empty character string for storage format
#'   (e.g., `"ducklake_table"`).
#' @param location_ref Single non-empty character string referencing the lake
#'   table or materialized asset (e.g., `"qc_lake.qc_regions"`).
#' @param raw_path Optional character string path to the immutable raw source
#'   file, or `NA_character_` when not applicable. Defaults to `NA_character_`.
#' @param crs_epsg Optional integer EPSG code (e.g., `32198L`), or `NA_integer_`
#'   when non-spatial.
#' @param notes Optional character string describing the dataset.
#' @param catalog_table Qualified or unqualified target table name. Defaults to
#'   `"qc_lake.lake_catalog"`.
#'
#' @return Invisibly returns `TRUE`.
#' @export
#' @md
upsert_lake_catalog <- function(
  con,
  dataset_name,
  source_system,
  format_type,
  location_ref,
  raw_path = NA_character_,
  crs_epsg = NA_integer_,
  notes = NA_character_,
  catalog_table = "qc_lake.lake_catalog"
) {
  if (!inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
    cli::cli_abort("{.arg con} must be a valid DBI connection.")
  }
  assert_non_empty_string(dataset_name, "dataset_name")
  assert_non_empty_string(source_system, "source_system")
  assert_non_empty_string(format_type, "format_type")
  assert_non_empty_string(location_ref, "location_ref")
  assert_non_empty_string(catalog_table, "catalog_table")

  if (!is.na(raw_path)) {
    assert_non_empty_string(raw_path, "raw_path")
  }

  raw_path_sql <- if (is.na(raw_path) || !nzchar(trimws(raw_path))) {
    "NULL::VARCHAR"
  } else {
    DBI::dbQuoteString(con, raw_path)
  }

  epsg_sql <- if (is.na(crs_epsg)) {
    "NULL::INTEGER"
  } else {
    as.integer(crs_epsg)
  }

  notes_sql <- if (is.na(notes) || !nzchar(trimws(notes))) {
    "NULL::VARCHAR"
  } else {
    DBI::dbQuoteString(con, notes)
  }

  dataset_sql <- DBI::dbQuoteString(con, dataset_name)
  source_sql <- DBI::dbQuoteString(con, source_system)
  format_sql <- DBI::dbQuoteString(con, format_type)
  location_sql <- DBI::dbQuoteString(con, location_ref)

  DBI::dbWithTransaction(con, {
    DBI::dbExecute(
      con,
      sprintf(
        "DELETE FROM %s WHERE dataset_name = %s;",
        catalog_table,
        dataset_sql
      )
    )

    DBI::dbExecute(
      con,
      sprintf(
        "
        INSERT INTO %s (
          dataset_name,
          source_system,
          format_type,
          location_ref,
          raw_path,
          crs_epsg,
          last_ingested_at,
          notes
        )
        VALUES (%s, %s, %s, %s, %s, %s, CURRENT_TIMESTAMP, %s);
        ",
        catalog_table,
        dataset_sql,
        source_sql,
        format_sql,
        location_sql,
        raw_path_sql,
        epsg_sql,
        notes_sql
      )
    )
  })

  invisible(TRUE)
}
