# tests/testthat/test-check_catalog.R
test_that("check_catalog passes valid catalog and catches invalid license", {
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(
    c(
      "datasets:",
      "  admin_muni:",
      "    title: \"Découpages administratifs\"",
      "    source: \"Données Québec\"",
      "    raw_path: \"data/raw/admin.gpkg\"",
      "    crs: \"EPSG:32198\"",
      "    license: \"CC-BY 4.0\"",
      "    notes: \"Vector boundaries\""
    ),
    tmp
  )

  expect_true(check_catalog(tmp))

  writeLines(
    c(
      "datasets:",
      "  admin_muni:",
      "    title: \"Découpages administratifs\"",
      "    source: \"Données Québec\"",
      "    raw_path: \"data/raw/admin.gpkg\"",
      "    crs: \"EPSG:32198\"",
      "    license: \"UNKNOWN_LICENSE\"",
      "    notes: \"Vector boundaries\""
    ),
    tmp
  )

  expect_error(check_catalog(tmp), "Invalid 'license'")
})
