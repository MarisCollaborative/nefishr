test_that("get_assessment_vals() extracts the assessment values", {
  expect_s3_class(get_assessment_vals(species = "Atlantic cod", year = 2024), "data.frame")
  expect_shape(get_assessment_vals(species = "Atlantic cod", year = 2024), ncol = 27)
})
