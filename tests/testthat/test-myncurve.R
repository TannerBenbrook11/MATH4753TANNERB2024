test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("mu_test_works", {
  result <- myncurve(mu = 10, sigma = 5, a = 6)

  # Check if the mu and sigma are correct
  expect_equal(result$mu, 10)

  # Check if the area is approximately equal to the expected value
  expect_equal(result$area, pnorm(6, mean = 10, sd = 5), tolerance = 1e-6) # Use tolerance for floating-point comparison
})

test_that("sigma_test_works", {
  result <- myncurve(mu = 10, sigma = 5, a = 6)

  # Check if the mu and sigma are correct
  expect_equal(result$sigma, 5)

  # Check if the area is approximately equal to the expected value
  expect_equal(result$area, pnorm(6, mean = 10, sd = 5), tolerance = 1e-6) # Use tolerance for floating-point comparison
})
