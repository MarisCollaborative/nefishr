test_that("get_species_code() finds the ITIS code needed to run analyses based on data in CAMS", {
  expect_s3_class(get_species_code(species = "cod"), "data.frame")
  expect_shape(get_species_code(species = "cod"), ncol = 3)
})

