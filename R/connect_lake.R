#' Load DuckDB official and community extensions on a connection
#'
#' Installs and loads standard DuckDB extensions as well as community
#' extensions (`FROM community`). When `"spatial"` is included in `extensions`,
#' sets `geometry_always_xy = true` for consistent `(X, Y)` / `(lon, lat)`
#' coordinate ordering.
#'
#' @param con A valid `DBIConnection` to a DuckDB instance.
#' @param extensions Character vector of official DuckDB extensions to install
#'   and load.
#' @param community_extensions Character vector of community DuckDB
#'   extensions (`FROM community`) to install and load.
#' @importFrom DBI dbIsValid dbExecute
#' @return Invisibly returns `con`.
#' @keywords internal
load_duckdb_extensions <- function(
  con,
  extensions = c("ducklake", "spatial"),
  community_extensions = character()
) {
  if (!DBI::dbIsValid(con)) {
    stop("`con` is not a valid DBI connection.", call. = FALSE)
  }

  if (!is.null(extensions) && length(extensions) > 0L) {
    for (ext in extensions) {
      DBI::dbExecute(con, sprintf("INSTALL %s; LOAD %s;", ext, ext))
    }
  }

  if (!is.null(community_extensions) && length(community_extensions) > 0L) {
    for (ext in community_extensions) {
      DBI::dbExecute(
        con,
        sprintf("INSTALL %s FROM community; LOAD %s;", ext, ext)
      )
    }
  }

  has_spatial <- (!is.null(extensions) && "spatial" %in% extensions) ||
    (!is.null(community_extensions) && "spatial" %in% community_extensions)

  if (isTRUE(has_spatial)) {
    DBI::dbExecute(con, "SET geometry_always_xy = true;")
  }

  invisible(con)
}

#' Connect to a local DuckLake catalog
#'
#' Opens a DuckDB connection, installs and loads requested official and
#' community extensions via [load_duckdb_extensions()], attaches the specified
#' DuckLake catalog, and sets it as the active schema.
#'
#' @param lake_dir Path to the root directory of the data lake containing
#'   `catalog.ducklake` and `data/lake/`.
#' @param read_only Logical; if `TRUE` (default), attaches the catalog in
#'   `READ_ONLY` mode to prevent accidental mutations and allow concurrent reads.
#'   Set to `FALSE` only inside ingestion/pipeline scripts.
#' @param catalog_file Name of the DuckLake catalog file inside `lake_dir`.
#' @param data_subdir Relative path to the managed Parquet directory inside
#'   `lake_dir`.
#' @param alias Schema alias used when attaching the DuckLake catalog.
#' @param drv Database driver passed to [DBI::dbConnect()]. Defaults to
#'   `duckdb::duckdb()`.
#' @param dbdir Local DuckDB database location (`":memory:"` by default).
#' @param extensions Character vector of official DuckDB extensions to install
#'   and load. Defaults to `c("ducklake", "spatial")`.
#' @param community_extensions Character vector of community DuckDB
#'   extensions (`FROM community`) to install and load. Defaults to
#'   `character()`.
#'
#' @return A `DBIConnection` object connected to DuckDB with requested
#'   extensions loaded and the DuckLake catalog attached and active. Close with
#'   `DBI::dbDisconnect(con, shutdown = TRUE)`.
#' @importFrom DBI dbConnect dbIsValid dbDisconnect dbExecute
#' @importFrom duckdb duckdb
#' @examples
#' \dontrun{
#' con <- connect_lake(
#'   lake_dir = "path/to/qc-datalake",
#'   read_only = TRUE,
#'   community_extensions = "h3"
#' )
#' on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
#' DBI::dbGetQuery(con, "SELECT * FROM lake_catalog;")
#' }
#' @export
connect_lake <- function(
  lake_dir = ".",
  read_only = TRUE,
  catalog_file = "catalog.ducklake",
  data_subdir = "data/lake",
  alias = "qc_lake",
  drv = duckdb::duckdb(),
  dbdir = ":memory:",
  extensions = c("ducklake", "spatial"),
  community_extensions = character()
) {
  if (!is.character(lake_dir) || length(lake_dir) != 1L || !nzchar(lake_dir)) {
    stop("`lake_dir` must be a single non-empty path.", call. = FALSE)
  }
  if (!dir.exists(lake_dir)) {
    stop("Lake directory does not exist: ", lake_dir, call. = FALSE)
  }
  if (!is.logical(read_only) || length(read_only) != 1L || is.na(read_only)) {
    stop("`read_only` must be TRUE or FALSE.", call. = FALSE)
  }

  lake_dir_norm <- normalizePath(lake_dir, winslash = "/", mustWork = TRUE)
  catalog_path <- file.path(lake_dir_norm, catalog_file)
  data_path <- file.path(lake_dir_norm, data_subdir)

  if (isTRUE(read_only) && !file.exists(catalog_path)) {
    stop(
      "Catalog file not found for read-only connection: ",
      catalog_path,
      call. = FALSE
    )
  }

  con <- DBI::dbConnect(drv = drv, dbdir = dbdir)
  on.exit(
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )

  load_duckdb_extensions(
    con = con,
    extensions = extensions,
    community_extensions = community_extensions
  )

  attach_opts <- if (isTRUE(read_only)) {
    "(READ_ONLY)"
  } else if (!file.exists(catalog_path)) {
    sprintf("(DATA_PATH '%s/')", data_path)
  } else {
    ""
  }
  attach_sql <- if (nzchar(attach_opts)) {
    sprintf("ATTACH 'ducklake:%s' AS %s %s;", catalog_path, alias, attach_opts)
  } else {
    sprintf("ATTACH 'ducklake:%s' AS %s;", catalog_path, alias)
  }

  DBI::dbExecute(con, attach_sql)
  DBI::dbExecute(con, sprintf("USE %s;", alias))

  on.exit(NULL)
  con
}
