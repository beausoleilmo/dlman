#' Write a Raster as a Cloud Optimized GeoTIFF (COG) Using Native `terra`
#'
#' Writes a `terra::SpatRaster` or an existing raster file directly to a
#' Cloud Optimized GeoTIFF (`.tif`) using `terra::writeRaster(..., filetype = "COG")`.
#'
#' @param x A `terra::SpatRaster` object or a single non-empty path to an
#'   existing raster file.
#' @param cog_path Single non-empty character string specifying the output `.tif`
#'   file path.
#' @param compress Compression method passed to GDAL's `COG` driver (e.g.,
#'   `"DEFLATE"`, `"ZSTD"`, `"LZW"`). Defaults to `"DEFLATE"`.
#' @param blocksize Integer tile width and height in pixels. Defaults to `512L`.
#' @param resampling Overview resampling method passed to GDAL's `COG` driver
#'   (e.g., `"AVERAGE"`, `"NEAREST"`, `"MODE"`). Defaults to `"AVERAGE"`.
#' @param overwrite Logical; whether to overwrite `cog_path` if it already
#'   exists. Defaults to `FALSE`.
#'
#' @return Invisibly returns the normalized output path `cog_path`.
#' @export
#' @md
write_cog_raster <- function(
  x,
  cog_path,
  compress = "DEFLATE",
  blocksize = 512L,
  resampling = "AVERAGE",
  overwrite = FALSE
) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg terra} is required to write COG rasters.")
  }

  assert_non_empty_string(cog_path, "cog_path")
  assert_non_empty_string(compress, "compress")
  assert_non_empty_string(resampling, "resampling")

  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    cli::cli_abort(
      "{.arg overwrite} must be a single non-missing logical value."
    )
  }

  blocksize <- as.integer(blocksize)
  if (length(blocksize) != 1L || is.na(blocksize) || blocksize <= 0L) {
    cli::cli_abort("{.arg blocksize} must be a single positive integer.")
  }

  if (file.exists(cog_path) && !overwrite) {
    cli::cli_abort(
      "Output COG already exists at {.path {cog_path}}; set {.arg overwrite = TRUE} to replace."
    )
  }

  r <- if (inherits(x, "SpatRaster")) {
    x
  } else if (is.character(x)) {
    assert_non_empty_string(x, "x")
    if (!file.exists(x)) {
      cli::cli_abort("Input raster file does not exist at {.path {x}}.")
    }
    terra::rast(normalizePath(x, winslash = "/", mustWork = TRUE))
  } else {
    cli::cli_abort("{.arg x} must be a {.cls SpatRaster} or a file path.")
  }

  dir.create(dirname(cog_path), recursive = TRUE, showWarnings = FALSE)
  out_path <- normalizePath(cog_path, winslash = "/", mustWork = FALSE)

  gdal_opts <- c(
    sprintf("BLOCKSIZE=%d", blocksize),
    sprintf("COMPRESS=%s", compress),
    sprintf("RESAMPLING=%s", resampling),
    "BIGTIFF=IF_SAFER"
  )

  terra::writeRaster(
    r,
    filename = out_path,
    filetype = "COG",
    gdal = gdal_opts,
    overwrite = overwrite
  )

  invisible(out_path)
}
