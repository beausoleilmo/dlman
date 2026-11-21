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
