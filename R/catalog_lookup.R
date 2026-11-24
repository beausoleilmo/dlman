#' Read Entries from the Technical Lake Catalog
#'
#' Queries `lake_catalog` to inspect available datasets and their metadata.
#'
#' @param con A valid `DBI` connection to DuckDB / DuckLake.
#' @param catalog_table Character string naming the catalog table. Defaults to
#'   `"lake_catalog"`.
#' @param dataset_name Optional dataset name to filter to a single entry.
#'
#' @return A `data.frame` of catalog rows.
#' @export
#' @md
read_lake_catalog <- function(
  con,
  catalog_table = "lake_catalog",
  dataset_name = NULL
) {
  if (!inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
    cli::cli_abort("{.arg con} must be a valid DBI connection.")
  }
  assert_non_empty_string(catalog_table, "catalog_table")

  tbl_sql <- DBI::dbQuoteIdentifier(con, catalog_table)

  if (!is.null(dataset_name)) {
    assert_non_empty_string(dataset_name, "dataset_name")
    val_sql <- DBI::dbQuoteString(con, dataset_name)
    return(
      DBI::dbGetQuery(
        con,
        sprintf("SELECT * FROM %s WHERE dataset_name = %s;", tbl_sql, val_sql)
      )
    )
  }

  DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM %s ORDER BY dataset_name;", tbl_sql)
  )
}

#' Read a Reference Lookup CSV with Encoding/Delimiter Checks & Warnings
#'
#' Reads a lookup file from `catalog/lookups/`, defaulting to UTF-8 and comma
#' separation while detecting and warning on non-UTF-8 encodings or semicolon
#' (`csv2`) delimiters.
#'
#' @param name Lookup name (with or without `.csv`).
#' @param lookups_dir Path to lookups directory. Defaults to `"catalog/lookups"`.
#' @param delim Field delimiter. If `NULL` (default), auto-detects `,`, `;`, or
#'   `\t` and warns if not `,`. Pass `","` or `";"` to force explicitly.
#' @param encoding Encoding name. If `NULL` (default), prefers `"UTF-8"` and
#'   falls back to guessed/common encodings (`"Windows-1252"`, `"Latin1"`) with a
#'   warning.
#' @param col_types `readr` column specification. Defaults to character columns to
#'   preserve leading zeros in codes.
#'
#' @return A `tibble` of the lookup table.
#' @export
#' @md
read_lookup_csv <- function(
  name,
  lookups_dir = "catalog/lookups",
  delim = NULL,
  encoding = NULL,
  col_types = readr::cols(.default = readr::col_character())
) {
  assert_non_empty_string(name, "name")
  assert_non_empty_string(lookups_dir, "lookups_dir")

  file_name <- if (grepl("\\.csv$", name, ignore.case = TRUE)) {
    name
  } else {
    paste0(name, ".csv")
  }

  path <- file.path(lookups_dir, file_name)
  if (!file.exists(path)) {
    cli::cli_abort("Lookup file not found at {.path {path}}.")
  }

  # 1. Resolve encoding + warn if non-UTF-8
  enc_used <- resolve_lookup_encoding(path, encoding = encoding)

  # 2. Resolve delimiter + warn if not comma
  delim_used <- resolve_lookup_delimiter(
    path,
    encoding = enc_used,
    delim = delim
  )

  out <- readr::read_delim(
    file = path,
    delim = delim_used,
    col_types = col_types,
    locale = readr::locale(encoding = enc_used),
    trim_ws = TRUE,
    show_col_types = FALSE
  )

  if (ncol(out) == 1L && delim_used == ",") {
    cli::cli_warn(
      "Lookup {.path {file_name}} parsed into a single column; check if it uses `;` or another delimiter."
    )
  }

  out
}

resolve_lookup_encoding <- function(path, encoding = NULL) {
  if (!is.null(encoding)) {
    assert_non_empty_string(encoding, "encoding")
    if (!toupper(encoding) %in% c("UTF-8", "UTF8")) {
      cli::cli_warn(
        "Reading {.path {basename(path)}} with explicit encoding {.val {encoding}} instead of {.val UTF-8}."
      )
    }
    return(encoding)
  }

  guesses <- tryCatch(
    readr::guess_encoding(path, n_max = 10000),
    error = function(e) NULL
  )

  top_enc <- if (!is.null(guesses) && nrow(guesses) > 0L) {
    guesses$encoding[[1L]]
  } else {
    "UTF-8"
  }

  if (!toupper(top_enc) %in% c("UTF-8", "UTF8", "ASCII")) {
    cli::cli_warn(
      c(
        "Non-UTF-8 encoding detected in {.path {basename(path)}} ({.val {top_enc}}).",
        "i" = "Standardize lookup files to UTF-8 in {.path catalog/lookups/} to avoid encoding drift."
      )
    )
    return(top_enc)
  }

  "UTF-8"
}

resolve_lookup_delimiter <- function(path, encoding = "UTF-8", delim = NULL) {
  if (!is.null(delim)) {
    assert_non_empty_string(delim, "delim")
    if (!identical(delim, ",")) {
      cli::cli_warn(
        "Using non-standard delimiter {.val {delim}} for {.path {basename(path)}} (expected {.val ,})."
      )
    }
    return(delim)
  }

  lines <- tryCatch(
    readLines(path, n = 10L, warn = FALSE, encoding = encoding),
    error = function(e) character(0)
  )
  lines <- lines[nzchar(trimws(lines))]

  if (length(lines) == 0L) {
    return(",")
  }

  candidates <- c(",", ";", "\t")
  scores <- vapply(
    candidates,
    function(d) {
      counts <- lengths(strsplit(lines, split = d, fixed = TRUE)) - 1L
      if (all(counts > 0L) && length(unique(counts)) == 1L) {
        counts[[1L]] * 100L
      } else if (any(counts > 0L)) {
        sum(counts)
      } else {
        -1L
      }
    },
    numeric(1)
  )

  best_delim <- candidates[[which.max(scores)]]
  if (max(scores) < 0L) {
    best_delim <- ","
  }

  if (!identical(best_delim, ",")) {
    cli::cli_warn(
      c(
        "Detected delimiter {.val {best_delim}} instead of {.val ,} in {.path {basename(path)}}.",
        "i" = "Consider re-saving lookup CSVs as comma-separated UTF-8 files."
      )
    )
  }

  best_delim
}


#' Normalize a Lookup CSV to Standard UTF-8 Comma Format
#'
#' Reads an existing lookup CSV (handling non-UTF-8 encodings or semicolon
#' delimiters via `read_lookup_csv()`) and re-writes it in standard UTF-8
#' comma-separated format.
#'
#' @param name Lookup name (with or without `.csv`).
#' @param lookups_dir Path to lookups directory. Defaults to `"catalog/lookups"`.
#' @param delim Optional explicit delimiter override passed to `read_lookup_csv()`.
#' @param encoding Optional explicit encoding override passed to `read_lookup_csv()`.
#' @param col_types `readr` column specification. Defaults to character columns so
#'   codes preserve leading zeros.
#' @param overwrite Logical flag; if `FALSE` (default), writes to `<name>_utf8.csv`
#'   unless `overwrite = TRUE` is explicitly requested.
#'
#' @return Invisibly returns the normalized output file path.
#' @export
#' @md
normalize_lookup_csv <- function(
  name,
  lookups_dir = "catalog/lookups",
  delim = NULL,
  encoding = NULL,
  col_types = readr::cols(.default = readr::col_character()),
  overwrite = FALSE
) {
  assert_non_empty_string(name, "name")
  assert_non_empty_string(lookups_dir, "lookups_dir")
  assert_flag(overwrite, "overwrite")

  base_name <- sub("\\.csv$", "", name, ignore.case = TRUE)
  src_path <- file.path(lookups_dir, paste0(base_name, ".csv"))

  if (!file.exists(src_path)) {
    cli::cli_abort("Lookup file not found at {.path {src_path}}.")
  }

  df <- read_lookup_csv(
    name = base_name,
    lookups_dir = lookups_dir,
    delim = delim,
    encoding = encoding,
    col_types = col_types
  )

  out_path <- if (isTRUE(overwrite)) {
    src_path
  } else {
    file.path(lookups_dir, paste0(base_name, "_utf8.csv"))
  }

  readr::write_csv(df, file = out_path, na = "")
  cli::cli_inform("Wrote normalized lookup to {.path {out_path}}.")

  invisible(out_path)
}
