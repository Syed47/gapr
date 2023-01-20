library(gapr)

test_that("load_gapr produces valid data", {
  expect_output(str(load_gapr("country", "Ireland")), "List of 3")
  expect_error(load_gapr("planet", "Mars"))
  expect_s3_class(load_gapr("country", "Ireland"), "gapr")
  # This test below should fail (on purpose)
  # expect_output(str(load_gapr("country", "Ireland")), "List of 2")
})
