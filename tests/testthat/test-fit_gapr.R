library(gapr)

test_that("fit_gapr produces valid list of data", {
  expect_output(str(fit(dat, effects = "mixed")), "List of 3")
  expect_error(fit(dat, effects = "wrong-effects"))
  expect_s3_class(fit(dat, effects = "mixed"), "gapr_fit")
  # This test below should fail (on purpose)
  # expect_output(str(fit(dat, effects = "mixed")), "List of 4")
})
