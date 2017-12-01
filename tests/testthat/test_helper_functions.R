library(reinforcelearn)
context("actorCritic")

test_that("softmax implementation is correct", {
  x = matrix(1:3, ncol = 3)
  numerator = sum(exp(x))
  expect_equal(softmax(x), matrix(c(exp(1) / numerator, exp(2) / numerator, exp(3) / numerator), ncol = 3))
})

test_that("identity2 returns its first argument", {
  expect_equal(identity2(2, 1), 2)
  expect_equal(identity2(7, ""), identity(7))
})


test_that("nHot returns n hot vector", {
  expect_equal(nHot(2, 4, out = "matrix"), matrix(c(0, 1, 0, 0), ncol = 4))
  expect_equal(nHot(1, 2, out = "vector"), c(1, 0))
})
