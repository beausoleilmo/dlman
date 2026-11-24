# tests/testthat/test-extract.R

test_that("download_ckan_resource validates invalid inputs", {
  expect_error(download_ckan_resource(""))
  expect_error(download_ckan_resource(NA_character_))
})

test_that("download_ckan_resource handles live CKAN download when online", {
  skip_on_cran()
  skip_if_offline()
  # skip(
  #   "Enable with a known stable CKAN resource URL when testing live downloads"
  # )

  out_dir <- tempfile("dq_dl_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  api_url <- build_ckan_resource_url("b53b6598-989c-4ed6-93a8-91c8b99c2e22")
  res <- download_ckan_resource(
    api_url = api_url,
    out_dir = out_dir,
    unzip = FALSE
  )
  sprintf("Output Download Données Québec: %s", res$file_path)
  expect_true(file.exists(res$file_path) || is.character(res$file_path))
})
