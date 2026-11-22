assert_non_empty_string <- function(x, arg_name = rlang::caller_arg(x)) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    cli::cli_abort(
      "{.arg {arg_name}} must be a single non-empty character string."
    )
  }
  invisible(TRUE)
}

assert_flag <- function(x, arg_name = rlang::caller_arg(x)) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg_name}} must be {.code TRUE} or {.code FALSE}.")
  }
  invisible(TRUE)
}
