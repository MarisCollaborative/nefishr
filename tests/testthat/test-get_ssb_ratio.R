test_that("get_ssb_ratio() extracts the ssb ratio", {
  expect_s3_class(get_ssb_ratio(species = "Atlantic cod", year = 2024), "data.frame")
  expect_output(str(get_ssb_ratio(species = "Atlantic cod", year = 2024)), "$ b_bmsy", fixed = TRUE)
})
