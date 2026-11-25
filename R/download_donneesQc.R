# Fonctions pour obtenir des données du portail de Données Québec
# Fait avec l'aide de Gemini.

#' Build a CKAN Action API Resource URL
#'
#' @description
#' Constructs a CKAN Action API endpoint URL (such as `resource_show`) for a
#' given resource identifier on Données Québec or another CKAN-compatible portal.
#'
#' @param resource_id A single non-empty character string identifying the CKAN
#'   resource.
#' @param base_url A single non-empty character string giving the base CKAN
#'   Action API URL. Defaults to
#'   `"https://www.donneesquebec.ca/recherche/api/3/action"`.
#' @param action A single non-empty character string naming the CKAN API action.
#'   Defaults to `"resource_show"`.
#'
#' @return A single character string containing the encoded CKAN API URL.
#'
#' @examples
#' build_ckan_resource_url("3448b141-f236-4c1b-add8-fe59109ac4f7")
#'
#' @export
#' @md
build_ckan_resource_url <- function(
  resource_id,
  base_url = "https://www.donneesquebec.ca/recherche/api/3/action",
  action = "resource_show"
) {
  assert_non_empty_string(resource_id, "resource_id")
  assert_non_empty_string(base_url, "base_url")
  assert_non_empty_string(action, "action")

  base_clean <- sub("/+$", "", trimws(base_url))
  action_clean <- sub("^/+|/+$", "", trimws(action))
  id_encoded <- utils::URLencode(trimws(resource_id), reserved = TRUE)

  paste0(base_clean, "/", action_clean, "?id=", id_encoded)
}

#' Download a CKAN Resource File
#'
#' @description
#' Queries a CKAN resource metadata endpoint, validates the response, downloads
#' the underlying resource file to disk using a standardized filename, prints
#' path and filename details, and optionally extracts `.zip` archives.
#'
#' @param api_url A single non-empty character string pointing to a CKAN
#'   `resource_show` URL (for example, produced by [build_ckan_resource_url()]).
#' @param out_dir A single non-empty character string specifying the output
#'   directory for the downloaded file. Defaults to a temporary directory created
#'   with [base::tempfile()].
#' @param dataset_slug Optional single non-empty character string used as the
#'   standardized filename prefix (ideally matching the target lake table name).
#'   When `NULL`, falls back to the CKAN resource name or original file stem.
#' @param unzip A single logical (`TRUE` or `FALSE`) indicating whether `.zip`
#'   archives should be extracted. Defaults to `TRUE`.
#' @param extract_dir Optional single non-empty character string specifying where
#'   `.zip` archives are extracted. Defaults to a temporary directory via
#'   [base::tempfile()] when `unzip = TRUE`, or `NULL` when `unzip = FALSE`.
#' @param user_agent A single non-empty character string sent as the HTTP
#'   `User-Agent` header. Defaults to `"R-donneesquebec-client"`.
#'
#' @return A list (invisibly) containing:
#'   * `out_dir`: Path to the output directory.
#'   * `file_path`: Path to the downloaded file.
#'   * `original_file_name`: Original filename derived from the download URL
#'     (or fallback).
#'   * `new_file_name`: Standardized filename written to `out_dir`.
#'   * `extract_dir`: Path to the extraction directory (or `NULL` if not unzipped).
#'   * `extracted_files`: Character vector of extracted file paths (empty if not
#'     unzipped or not a `.zip` file).
#'   * `metadata`: Parsed CKAN resource metadata list.
#'
#' @examples
#' \dontrun{
#' api_url <- build_ckan_resource_url("3448b141-f236-4c1b-add8-fe59109ac4f7")
#' res <- download_ckan_resource(
#'   api_url,
#'   out_dir = "raw/donnees_quebec/decoupages_administratifs",
#'   dataset_slug = "decoupages_administratifs"
#' )
#' if (!is.null(res$extract_dir) && !identical(res$extract_dir, res$out_dir)) {
#'   on.exit(
#'     unlink(res$extract_dir, recursive = TRUE, force = TRUE),
#'     add = TRUE
#'   )
#' }
#' res$extracted_files
#' }
#'
#' @export
#' @md
download_ckan_resource <- function(
  api_url,
  out_dir = tempfile("dq_admin_"),
  dataset_slug = NULL,
  unzip = TRUE,
  extract_dir = if (isTRUE(unzip)) tempfile("dq_extract_") else NULL,
  user_agent = "R-donneesquebec-client"
) {
  assert_non_empty_string(api_url, "api_url")
  assert_non_empty_string(out_dir, "out_dir")
  if (!is.null(dataset_slug)) {
    assert_non_empty_string(dataset_slug, "dataset_slug")
  }
  assert_non_empty_string(user_agent, "user_agent")
  assert_flag(unzip, "unzip")
  if (!is.null(extract_dir)) {
    assert_non_empty_string(extract_dir, "extract_dir")
  }

  meta <- perform_ckan_get(api_url = api_url, user_agent = user_agent)

  if (!isTRUE(meta$success)) {
    cli::cli_abort("CKAN API request failed for {.url {api_url}}.")
  }

  res_info <- meta$result
  download_url <- res_info$url

  if (
    !is.character(download_url) ||
      length(download_url) != 1L ||
      !nzchar(trimws(download_url))
  ) {
    cli::cli_abort(
      "CKAN metadata for {.url {api_url}} did not include a valid download URL."
    )
  }

  resource_label <- res_info$name %||% "unnamed resource"
  resource_fmt <- res_info$format %||% "unknown format"
  est_bytes <- suppressWarnings(as.numeric(res_info$size %||% NA_real_))
  size_note <- if (
    length(est_bytes) == 1L && !is.na(est_bytes) && est_bytes >= 0
  ) {
    paste0(
      " [",
      format(structure(est_bytes, class = "object_size"), units = "auto"),
      "]"
    )
  } else {
    ""
  }

  original_file_name <- extract_original_filename(download_url)
  new_file_name <- standardize_download_filename(
    original_name = original_file_name,
    dataset_slug = dataset_slug,
    resource_name = resource_label,
    resource_fmt = resource_fmt,
    stamp_date = Sys.Date()
  )

  # Create output directory if it doesn't exist yet (works for temp and user folders)
  dir_created <- dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir_created && !dir.exists(out_dir)) {
    cli::cli_abort("Could not create output directory at {.path {out_dir}}.")
  }
  # Get file size
  size_out <- get_ckan_resource_size(api_url)
  size_label <- if (is.null(size_out) || is.na(size_out)) {
    "unknown"
  } else {
    format(
      structure(as.numeric(size_out), class = "object_size"),
      units = "auto"
    )
  }
  archive_path <- file.path(out_dir, new_file_name)

  cli::cli_inform(c(
    "----------------",
    "Fetching resource:",
    " {resource_label} ({resource_fmt}){size_note}.",
    # "i" = "Output directory: {.path {out_dir}}",
    "i" = "Original file name: {.file {original_file_name}}",
    "i" = "File size: {size_label}",
    "i" = "Output path: {.path {archive_path}}"
    # "i" = "New file name: {.file {new_file_name}}",
  ))

  tictoc::tic("Download time: ")
  on.exit(try(tictoc::toc(), silent = TRUE), add = TRUE)

  perform_ckan_get(
    api_url = download_url,
    user_agent = user_agent,
    path = archive_path
  )

  on.exit(NULL)
  tictoc::toc()

  if (!file.exists(archive_path) || isTRUE(file.info(archive_path)$size <= 0)) {
    cli::cli_abort(
      "Downloaded file at {.path {archive_path}} is missing or empty."
    )
  }

  extracted_files <- character(0)
  used_extract_dir <- NULL

  is_zip <- grepl("\\.zip$", archive_path, ignore.case = TRUE) ||
    identical(toupper(trimws(resource_fmt)), "ZIP")

  if (isTRUE(unzip) && is_zip) {
    used_extract_dir <- extract_dir %||% tempfile("dq_extract_")

    ext_created <- dir.create(
      used_extract_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    if (!ext_created && !dir.exists(used_extract_dir)) {
      cli::cli_abort(
        "Could not create extraction directory at {.path {used_extract_dir}}."
      )
    }

    extracted_files <- utils::unzip(archive_path, exdir = used_extract_dir)
  }

  invisible(list(
    out_dir = out_dir,
    file_path = archive_path,
    original_file_name = original_file_name,
    new_file_name = new_file_name,
    extract_dir = used_extract_dir,
    extracted_files = extracted_files,
    metadata = res_info
  ))
}

# --- Filename Standardization Helpers ----------------------------------------

#' @noRd
extract_original_filename <- function(download_url) {
  clean_url <- sub("[?#].*$", "", download_url)
  file_name <- basename(clean_url)

  if (
    !nzchar(file_name) ||
      identical(file_name, "/") ||
      identical(file_name, ".")
  ) {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    return(paste0("resource_", timestamp))
  }

  utils::URLdecode(file_name)
}

#' @noRd
slugify_ascii <- function(x, fallback = "resource") {
  if (!is.character(x) || length(x) != 1L || !nzchar(trimws(x))) {
    return(fallback)
  }

  from_chars <- paste0(
    "\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5",
    "\u00e7",
    "\u00e8\u00e9\u00ea\u00eb",
    "\u00ec\u00ed\u00ee\u00ef",
    "\u00f1",
    "\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8",
    "\u00f9\u00fa\u00fb\u00fc",
    "\u00fd\u00ff"
  )
  to_chars <- paste0(
    "aaaaaa",
    "c",
    "eeee",
    "iiii",
    "n",
    "oooooo",
    "uuuu",
    "yy"
  )

  slug <- trimws(x) |>
    sub("\\s*\\(format\\s+[^)]+\\)\\s*$", "", x = _, ignore.case = TRUE) |>
    tolower() |>
    gsub("\u00e6", "ae", x = _, fixed = TRUE) |>
    gsub("\u0153", "oe", x = _, fixed = TRUE) |>
    chartr(from_chars, to_chars, x = _) |>
    gsub("[^a-z0-9]+", "_", x = _) |>
    gsub("^_+|_+$", "", x = _)

  if (nzchar(slug)) slug else fallback
}

#' @noRd
standardize_download_filename <- function(
  original_name,
  dataset_slug = NULL,
  resource_name = NULL,
  resource_fmt = NULL,
  stamp_date = Sys.Date(),
  sep = "_"
) {
  parts <- split_archive_extension(original_name, resource_fmt = resource_fmt)

  prefix_source <- if (!is.null(dataset_slug) && nzchar(trimws(dataset_slug))) {
    dataset_slug
  } else if (
    !is.null(resource_name) && !identical(resource_name, "unnamed resource")
  ) {
    resource_name
  } else {
    parts$stem
  }

  slug <- slugify_ascii(prefix_source, fallback = "resource")
  date_str <- format(as.Date(stamp_date), "%Y-%m-%d")

  # Avoid duplicating the date if already present at the end of the slug
  stem <- if (endsWith(slug, paste0(sep, date_str))) {
    slug
  } else {
    paste(slug, date_str, sep = sep)
  }

  paste0(stem, parts$ext)
}

#' @noRd
split_archive_extension <- function(file_name, resource_fmt = NULL) {
  lower_name <- tolower(file_name)

  # Preserve common compound extensions first
  compound_exts <- c(".gpkg.zip", ".shp.zip", ".geojson.zip", ".tar.gz")
  for (ext in compound_exts) {
    if (endsWith(lower_name, ext)) {
      stem <- substr(file_name, 1L, nchar(file_name) - nchar(ext))
      return(list(stem = stem, ext = ext))
    }
  }

  dot_pos <- regexpr("\\.[A-Za-z0-9]+$", file_name)
  if (dot_pos > 1L) {
    stem <- substr(file_name, 1L, dot_pos - 1L)
    ext <- tolower(substr(file_name, dot_pos, nchar(file_name)))
    return(list(stem = stem, ext = ext))
  }

  fmt_slug <- slugify_ascii(resource_fmt, fallback = "")
  ext <- if (nzchar(fmt_slug)) paste0(".", fmt_slug) else ""
  list(stem = file_name, ext = ext)
}

#' Fetch the Données Québec Dataset Catalog
#'
#' @description
#' Queries a CKAN `package_search` endpoint in pages and returns a data frame
#' containing dataset identifiers, slugs, titles, and resource counts.
#'
#' @param base_url A single non-empty character string giving the base CKAN
#'   Action API URL. Defaults to
#'   `"https://www.donneesquebec.ca/recherche/api/3/action"`.
#' @param rows A single positive integer specifying how many datasets to fetch
#'   per page. Defaults to `500L`.
#' @param user_agent A single non-empty character string sent as the HTTP
#'   `User-Agent` header. Defaults to `"R-donneesquebec-client"`.
#'
#' @return A `data.frame` with columns `id`, `name`, `title`, and
#'   `n_resources`.
#'
#' @examples
#' \dontrun{
#' catalog <- fetch_dq_catalog()
#' head(catalog, 10)
#' }
#'
#' @export
#' @md
fetch_dq_catalog <- function(
  base_url = "https://www.donneesquebec.ca/recherche/api/3/action",
  rows = 500L,
  user_agent = "R-donneesquebec-client"
) {
  assert_non_empty_string(base_url, "base_url")
  assert_non_empty_string(user_agent, "user_agent")
  rows_int <- assert_positive_int(rows, "rows")

  clean_base <- sub("/+$", "", trimws(base_url))
  start <- 0L
  total_count <- NA_integer_
  pages <- list()

  repeat {
    query_url <- paste0(
      clean_base,
      "/package_search?rows=",
      rows_int,
      "&start=",
      start
    )

    resp <- perform_ckan_get(api_url = query_url, user_agent = user_agent)

    if (!isTRUE(resp$success)) {
      cli::cli_abort("CKAN {.code package_search} failed at offset {start}.")
    }

    result <- resp$result
    if (is.na(total_count)) {
      total_count <- as.integer(result$count %||% 0L)
    }

    batch <- result$results %||% list()
    if (length(batch) == 0L) {
      break
    }

    pages[[length(pages) + 1L]] <- parse_catalog_page(batch)
    start <- start + length(batch)

    if (start >= total_count) {
      break
    }
  }

  if (length(pages) == 0L) {
    return(empty_catalog_df())
  }

  do.call(rbind, pages)
}


#' Perform a GET request against a CKAN endpoint or resource URL
#'
#' Sends an HTTP `GET` request with a custom `User-Agent` header via
#' [httr2::request()] and [httr2::req_perform()]. When `path` is supplied, the
#' response body is written directly to disk (with optional download progress)
#' and `path` is returned invisibly; when `path` is `NULL`, the JSON response
#' body is parsed and returned.
#'
#' @param api_url Character scalar giving the CKAN API endpoint or direct
#'   resource download URL.
#' @param user_agent Character scalar specifying the `User-Agent` string sent
#'   with the request.
#' @param path Optional file path where the downloaded response body should be
#'   written. If `NULL` (default), parses the response as JSON instead of saving
#'   to disk.
#' @param progress Logical flag controlling whether download progress is shown
#'   when `path` is non-`NULL`. Defaults to `interactive()`.
#'
#' @return When `path` is provided, the file path `path` returned invisibly.
#'   When `path` is `NULL`, the parsed JSON response returned by
#'   [httr2::resp_body_json()].
#' @noRd
perform_ckan_get <- function(
  api_url,
  user_agent,
  path = NULL,
  progress = interactive()
) {
  req <- httr2::request(api_url) |>
    httr2::req_user_agent(user_agent)

  if (!is.null(path)) {
    if (isTRUE(progress)) {
      req <- httr2::req_progress(req, type = "down")
    }
    httr2::req_perform(req, path = path)
    return(invisible(path))
  }

  httr2::req_perform(req) |>
    httr2::resp_body_json()
}

#' @noRd
resolve_download_path <- function(download_url, out_dir) {
  # Fall back to a generated filename if the download URL has no basename
  clean_url <- sub("[?#].*$", "", download_url)
  file_name <- basename(clean_url)

  if (
    !nzchar(file_name) ||
      identical(file_name, "/") ||
      identical(file_name, ".")
  ) {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_name <- paste0("resource_", timestamp)
  }

  file.path(out_dir, file_name)
}

#' Convert a page of CKAN dataset records into a data frame
#'
#' Extracts `id`, `name`, `title`, and resource count (`n_resources`) from a
#' list of CKAN dataset/package objects returned by a catalog search or listing
#' endpoint, substituting `NA_character_` when text fields are `NULL` via
#' `%||%` and `0L` when `resources` is absent.
#'
#' @param batch A list of CKAN dataset/package objects (typically from a
#'   paginated CKAN search or package list response), where each element may
#'   contain `id`, `name`, `title`, and `resources`.
#'
#' @return A `data.frame` matching empty_catalog_df() with columns `id`
#'   (`character`), `name` (`character`), `title` (`character`), and
#'   `n_resources` (`integer`), containing one row per element in `batch` (or
#'   zero rows if `batch` is empty).
#' @noRd
parse_catalog_page <- function(batch) {
  data.frame(
    id = vapply(batch, function(x) x$id %||% NA_character_, character(1)),
    name = vapply(batch, function(x) x$name %||% NA_character_, character(1)),
    title = vapply(batch, function(x) x$title %||% NA_character_, character(1)),
    n_resources = vapply(
      batch,
      function(x) length(x$resources %||% list()),
      integer(1)
    ),
    stringsAsFactors = FALSE
  )
}


#' Construct an empty typed data frame for catalog listings
#'
#' Creates a zero-row `data.frame` with explicit column types (`id`, `name`,
#' and `title` as `character`; `n_resources` as `integer`) so catalog search or
#' listing helpers return a consistent schema even when no matching records are
#' found.
#'
#' @return A 0-row `data.frame` with columns `id` (`character`), `name`
#'   (`character`), `title` (`character`), and `n_resources` (`integer`).
#' @noRd
empty_catalog_df <- function() {
  data.frame(
    id = character(0),
    name = character(0),
    title = character(0),
    n_resources = integer(0),
    stringsAsFactors = FALSE
  )
}


#' List Resources for a CKAN Dataset
#'
#' @description
#' Queries a CKAN `package_show` endpoint using a dataset identifier or slug and
#' returns a data frame of its associated resources, including resource IDs,
#' names, formats, and URLs.
#'
#' @param dataset_id_or_name A single non-empty character string specifying the
#'   dataset `id` (for example, from [fetch_dq_catalog()]) or dataset slug
#'   (`name`).
#' @param base_url A single non-empty character string giving the base CKAN
#'   Action API URL. Defaults to
#'   `"https://www.donneesquebec.ca/recherche/api/3/action"`.
#' @param user_agent A single non-empty character string sent as the HTTP
#'   `User-Agent` header. Defaults to `"R-donneesquebec-client"`.
#'
#' @return A `data.frame` with columns `resource_id`, `name`, `format`, and
#'   `url`.
#'
#' @examples
#' \dontrun{
#' res_tbl <- fetch_dataset_resources("decoupages-administratifs")
#' head(res_tbl)
#' }
#'
#' @export
#' @md
fetch_dataset_resources <- function(
  dataset_id_or_name,
  base_url = "https://www.donneesquebec.ca/recherche/api/3/action",
  user_agent = "R-donneesquebec-client"
) {
  assert_non_empty_string(dataset_id_or_name, "dataset_id_or_name")
  assert_non_empty_string(base_url, "base_url")
  assert_non_empty_string(user_agent, "user_agent")

  clean_base <- sub("/+$", "", trimws(base_url))
  encoded_id <- utils::URLencode(trimws(dataset_id_or_name), reserved = TRUE)
  query_url <- paste0(clean_base, "/package_show?id=", encoded_id)

  resp <- perform_ckan_get(api_url = query_url, user_agent = user_agent)

  if (!isTRUE(resp$success)) {
    cli::cli_abort(
      "CKAN {.code package_show} failed for {.val {dataset_id_or_name}}."
    )
  }

  resources <- resp$result$resources %||% list()
  if (length(resources) == 0L) {
    return(empty_resources_df())
  }

  parse_resources_list(resources)
}

# --- Internal Helpers --------------------------------------------------------
#' Convert a list of CKAN resource objects into a data frame
#'
#' Extracts `id`, `name`, `format`, and `url` from each element of `resources`
#' into a typed `data.frame`, substituting `NA_character_` when a field is
#' `NULL` via `%||%`.
#'
#' @param resources A list of resource metadata objects (typically parsed from
#'   a CKAN package JSON payload), where each element may contain `id`, `name`,
#'   `format`, and `url`.
#'
#' @return A `data.frame` with columns `resource_id` (`character`), `name`
#'   (`character`), `format` (`character`), and `url` (`character`), with one
#'   row per element in `resources` (or zero rows if `resources` is empty).
#' @noRd
parse_resources_list <- function(resources) {
  data.frame(
    resource_id = vapply(
      resources,
      function(r) r$id %||% NA_character_,
      character(1)
    ),
    name = vapply(
      resources,
      function(r) r$name %||% NA_character_,
      character(1)
    ),
    format = vapply(
      resources,
      function(r) r$format %||% NA_character_,
      character(1)
    ),
    url = vapply(
      resources,
      function(r) r$url %||% NA_character_,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

#' Construct an empty typed data frame for CKAN resources
#'
#' @description Creates a zero-row `data.frame` matching the schema produced by
#' parse_resources_list() (`resource_id`, `name`, `format`, and `url` as
#' `character` columns) so resource-listing helpers return a consistent table
#' structure when no resources are available.
#'
#' @return A 0-row `data.frame` with columns `resource_id` (`character`),
#'   `name` (`character`), `format` (`character`), and `url` (`character`).
#' @noRd
empty_resources_df <- function() {
  data.frame(
    resource_id = character(0),
    name = character(0),
    format = character(0),
    url = character(0),
    stringsAsFactors = FALSE
  )
}

#' Estimate CKAN Resource Download Size
#'
#' @description
#' Performs a quick, non-downloading scan of a CKAN resource's size in bytes
#' by checking CKAN `resource_show` metadata first and falling back to an HTTP
#' `HEAD` request (with a byte-range fallback when needed) against the file URL.
#'
#' @param api_url A single non-empty character string pointing to a CKAN
#'   `resource_show` URL (for example, produced by [build_ckan_resource_url()]).
#' @param user_agent A single non-empty character string sent as the HTTP
#'   `User-Agent` header. Defaults to `"R-donneesquebec-client"`.
#' @param timeout A single positive numeric value specifying the request timeout
#'   in seconds. Defaults to `10`.
#'
#' @return A single numeric value giving the estimated download size in bytes,
#'   or `NA_real_` if the server or metadata does not expose a size. Includes a
#'   `"source"` attribute (`"ckan_metadata"`, `"http_head"`, `"http_range"`,
#'   or `"unavailable"`).
#'
#' @examples
#' \dontrun{
#' api_url <- build_ckan_resource_url("3448b141-f236-4c1b-add8-fe59109ac4f7")
#' api_url <- build_ckan_resource_url("3f91562f-f0f0-463d-a0e8-0c3a622abb78")
#' bytes <- get_ckan_resource_size(api_url)
#' format(structure(bytes, class = "object_size"), units = "auto")
#' }
#'
#' @export
#' @md
get_ckan_resource_size <- function(
  api_url,
  user_agent = "R-donneesquebec-client",
  timeout = 10
) {
  assert_non_empty_string(api_url, "api_url")
  assert_non_empty_string(user_agent, "user_agent")
  if (
    !is.numeric(timeout) ||
      length(timeout) != 1L ||
      is.na(timeout) ||
      timeout <= 0
  ) {
    cli::cli_abort("{.arg timeout} must be a single positive number.")
  }

  meta <- perform_ckan_get(api_url = api_url, user_agent = user_agent)
  if (!isTRUE(meta$success)) {
    cli::cli_abort("CKAN API request failed for {.url {api_url}}.")
  }

  res_info <- meta$result %||% list()

  # 1. Fast path: CKAN metadata 'size' field (in bytes)
  meta_size <- suppressWarnings(as.numeric(res_info$size))
  if (length(meta_size) == 1L && !is.na(meta_size) && meta_size >= 0) {
    return(structure(
      meta_size,
      source = "ckan_metadata",
      class = "ckan_resource_size"
    ))
  }

  download_url <- res_info$url
  if (
    !is.character(download_url) ||
      length(download_url) != 1L ||
      !nzchar(trimws(download_url))
  ) {
    return(structure(
      NA_real_,
      source = "unavailable",
      class = "ckan_resource_size"
    ))
  }

  # 2. Quick scan via HTTP HEAD -> Content-Length
  head_bytes <- probe_url_content_length(
    url = download_url,
    user_agent = user_agent,
    timeout = timeout,
    method = "HEAD"
  )
  if (!is.na(head_bytes)) {
    return(structure(
      head_bytes,
      source = "http_head",
      class = "ckan_resource_size"
    ))
  }

  # 3. Fallback: 1-byte Range request -> Content-Range total ("bytes 0-0/12345")
  range_bytes <- probe_url_content_length(
    url = download_url,
    user_agent = user_agent,
    timeout = timeout,
    method = "GET",
    range_header = "bytes=0-0"
  )
  if (!is.na(range_bytes)) {
    return(structure(
      range_bytes,
      source = "http_range",
      class = "ckan_resource_size"
    ))
  }

  structure(NA_real_, source = "unavailable", class = "ckan_resource_size")
}

#' @keywords internal
#' @noRd
probe_url_content_length <- function(
  url,
  user_agent,
  timeout,
  method = c("HEAD", "GET"),
  range_header = NULL
) {
  method <- match.arg(method)

  req <- httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_timeout(timeout) |>
    httr2::req_method(method) |>
    httr2::req_error(is_error = function(resp) FALSE)

  if (!is.null(range_header)) {
    req <- httr2::req_headers(req, Range = range_header)
  }

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp)) {
    return(NA_real_)
  }

  status <- httr2::resp_status(resp)
  if (status >= 400L && status != 206L) {
    return(NA_real_)
  }

  cl <- suppressWarnings(as.numeric(
    httr2::resp_header(resp, "Content-Length") %||% NA_character_
  ))
  if (length(cl) == 1L && !is.na(cl) && cl >= 0) {
    # Note: for 206 Partial Content, Content-Length is the slice length; prefer Content-Range total
    if (status != 206L) {
      return(cl)
    }
  }

  cr <- httr2::resp_header(resp, "Content-Range") %||% NA_character_
  if (!is.na(cr) && grepl("/([0-9]+)$", cr)) {
    total_str <- sub("^.*/([0-9]+)$", "\\1", trimws(cr))
    total_num <- suppressWarnings(as.numeric(total_str))
    if (length(total_num) == 1L && !is.na(total_num) && total_num >= 0) {
      return(total_num)
    }
  }

  NA_real_
}

#' @export
format.ckan_resource_size <- function(x, units = "auto", ...) {
  if (length(x) == 0L || is.na(x)) {
    return("unknown size")
  }
  format(structure(as.numeric(x), class = "object_size"), units = units, ...)
}

#' @export
print.ckan_resource_size <- function(x, units = "auto", ...) {
  src <- attr(x, "source", exact = TRUE)
  formatted <- format.ckan_resource_size(x, units = units, ...)
  if (!is.null(src) && nzchar(src)) {
    cat(sprintf("%s [%s]\n", formatted, src))
  } else {
    cat(sprintf("%s\n", formatted))
  }
  invisible(x)
}
