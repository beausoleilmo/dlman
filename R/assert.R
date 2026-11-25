#' Assert that an input is a single non-empty character string
#'
#' Checks that `x` is a non-`NA` character scalar containing at least one
#' non-whitespace character after trimming. Aborts with a formatted `cli` error
#' if validation fails.
#'
#' @param x Object to validate.
#' @param arg_name Argument label used in the error message. Defaults to the
#'   caller expression via `rlang::caller_arg(x)`.
#'
#' @return Invisible `TRUE` when validation succeeds; otherwise throws an error
#'   via [cli::cli_abort()].
#' @export
assert_non_empty_string <- function(x, arg_name = rlang::caller_arg(x)) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    cli::cli_abort(
      "{.arg {arg_name}} must be a single non-empty character string."
    )
  }
  invisible(TRUE)
}

#' Assert that an input is a single non-missing logical flag
#'
#' Checks that `x` is a length-1 logical vector with no `NA` (`TRUE` or
#' `FALSE`). Aborts with a formatted `cli` error if validation fails.
#'
#' @param x Object to validate.
#' @param arg_name Argument label used in the error message. Defaults to the
#'   caller expression via `rlang::caller_arg(x)`.
#'
#' @return Invisible `TRUE` when validation succeeds; otherwise throws an error
#'   via [cli::cli_abort()].
#' @noRd
assert_flag <- function(x, arg_name = rlang::caller_arg(x)) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg_name}} must be {.code TRUE} or {.code FALSE}.")
  }
  invisible(TRUE)
}

#' Assert that an input is a single positive whole number
#'
#' Validates that `x` is a non-`NA` numeric scalar greater than or equal to `1`
#' with no fractional part, and returns it coerced to an `integer` scalar.
#' Aborts with a formatted `cli` error if validation fails. Note that
#' `Inf` or `-Inf` is rejected because `Inf %% 1` evaluates to `NaN`.
#'
#' @param x Object to validate.
#' @param arg_name Character scalar naming the argument in the error message
#'   passed to [cli::cli_abort()].
#'
#' @return A single `integer` (`length == 1L`) equal to `as.integer(x)` when
#'   validation succeeds; otherwise throws an error via [cli::cli_abort()].
#' @noRd
assert_positive_int <- function(x, arg_name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x %% 1 != 0) {
    cli::cli_abort("{.arg {arg_name}} must be a single positive integer.")
  }
  as.integer(x)
}


#' Assert that a dataset entry exists in the YAML catalog contract
#'
#' Reads a YAML catalog file (such as `catalog/dataset.yml`) and verifies that
#' `dataset_id` is present under `datasets`. Optionally checks required fields
#' on that dataset entry (for example, `format` or `crs`).
#'
#' @param catalog_path Path to the YAML catalog file.
#' @param dataset_id Character scalar naming the dataset key under `datasets`
#'   (for example, `"qc_lulc_2020"`).
#' @param required_fields Optional character vector of field names that must
#'   exist and be non-empty on the dataset entry.
#'
#' @return The dataset metadata list entry invisibly when present and valid;
#'   otherwise aborts with a `cli` error.
#' @export
assert_catalog_dataset <- function(
  catalog_path,
  dataset_id,
  required_fields = character()
) {
  assert_non_empty_string(catalog_path, "catalog_path")
  assert_non_empty_string(dataset_id, "dataset_id")

  if (!file.exists(catalog_path)) {
    cli::cli_abort("Catalog file not found at {.path {catalog_path}}.")
  }

  spec <- yaml::read_yaml(catalog_path)
  datasets <- spec$datasets

  if (is.null(datasets) || !is.list(datasets)) {
    cli::cli_abort(
      "Catalog {.path {catalog_path}} is missing a top-level {.field datasets} mapping."
    )
  }

  if (!dataset_id %in% names(datasets) || is.null(datasets[[dataset_id]])) {
    cli::cli_abort(
      c(
        "Dataset {.val {dataset_id}} is not defined in {.path {catalog_path}}.",
        "i" = "Add a {.field {dataset_id}} entry under {.field datasets} before running ingestion."
      )
    )
  }

  entry <- datasets[[dataset_id]]

  for (field in required_fields) {
    val <- entry[[field]]
    if (
      is.null(val) ||
        !is.character(val) ||
        length(val) != 1L ||
        !nzchar(trimws(val))
    ) {
      cli::cli_abort(
        "Dataset {.val {dataset_id}} in {.path {catalog_path}} is missing required field {.field {field}}."
      )
    }
  }

  invisible(entry)
}
