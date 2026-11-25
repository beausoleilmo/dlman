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
