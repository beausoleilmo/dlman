#' Initialize an ingestion session for a DuckLake repository
#'
#' Validates the YAML catalog contract and opens a read-write DuckLake
#' connection along with resolved directory paths for an ingestion script.
#'
#' @param lake_dir Path to the root directory of the data lake repository.
#' @param catalog_path Path to the dataset YAML catalog file.
#' @param raw_dir Path to the immutable raw source archive directory.
#' @param data_lake_path Path to the DuckLake managed Parquet storage directory.
#' @param alias Schema/catalog alias used inside DuckDB (e.g., `"qc_lake"`).
#' @param extensions Character vector of DuckDB extensions to load.
#' @param community_extensions Character vector of DuckDB community extensions
#'   to load (e.g., `"h3"`).
#'
#' @return A named list containing `con`, `lake_dir`, `catalog_path`,
#'   `raw_dir`, `data_lake_path`, and `alias`. Note that `on.exit()` cleanup
#'   should be registered in the calling ingestion script so the connection
#'   remains active for the full script duration.
#' @export
init_ingestion <- function(
  lake_dir = ".",
  catalog_path = file.path(lake_dir, "catalog", "dataset.yml"),
  raw_dir = file.path(lake_dir, "data", "raw"),
  data_lake_path = file.path(lake_dir, "data", "lake"),
  alias = "qc_lake",
  extensions = c("ducklake", "spatial"),
  community_extensions = character()
) {
  if (!is.character(lake_dir) || length(lake_dir) != 1L || !nzchar(lake_dir)) {
    stop("`lake_dir` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.character(alias) || length(alias) != 1L || !nzchar(alias)) {
    stop("`alias` must be a non-empty character scalar.", call. = FALSE)
  }

  check_catalog(catalog_path = catalog_path)

  con <- connect_lake(
    lake_dir = lake_dir,
    read_only = FALSE,
    alias = alias,
    extensions = extensions,
    community_extensions = community_extensions
  )

  list(
    con = con,
    lake_dir = lake_dir,
    catalog_path = catalog_path,
    raw_dir = raw_dir,
    data_lake_path = data_lake_path,
    alias = alias
  )
}

#' Connect to a DuckLake repository in read-only mode for analysis
#'
#' Opens a read-only DuckLake connection intended for exploratory queries,
#' reporting, and self-service analysis without taking write locks or mutating
#' lake state.
#'
#' @param lake_dir Path to the root directory of the data lake repository.
#' @param alias Schema/catalog alias used inside DuckDB (e.g., `"qc_lake"`).
#' @param extensions Character vector of DuckDB extensions to load.
#' @param community_extensions Character vector of DuckDB community extensions
#'   to load (e.g., `"h3"`).
#'
#' @return A read-only `DBI` connection object (`DBIConnection`).
#' @export
connect_lake_readonly <- function(
  lake_dir = ".",
  alias = "qc_lake",
  extensions = c("ducklake", "spatial"),
  community_extensions = character()
) {
  if (!is.character(lake_dir) || length(lake_dir) != 1L || !nzchar(lake_dir)) {
    stop("`lake_dir` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.character(alias) || length(alias) != 1L || !nzchar(alias)) {
    stop("`alias` must be a non-empty character scalar.", call. = FALSE)
  }

  connect_lake(
    lake_dir = lake_dir,
    read_only = TRUE,
    alias = alias,
    extensions = extensions,
    community_extensions = community_extensions
  )
}

#' Close a DuckLake connection safely
#'
#' Disconnects a `DBI` connection if it is still valid, shutting down the
#' underlying DuckDB instance cleanly.
#'
#' @param con A `DBI` connection object or `NULL`.
#'
#' @return Invisible `TRUE`.
#' @export
disconnect_lake <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  invisible(TRUE)
}
