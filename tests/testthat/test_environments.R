library(reinforcelearn)

context("test environments")


test_that("makeEnvironment function returns error if function arguments
  are not of the specified class", {
  expect_error(makeEnvironment(1234)) # error because of non-character gym.envir.name
  expect_error(makeEnvironment("CartPole-v0", render = 22)) # error because render is not logical
  expect_error(makeEnvironment(reward.matrix = matrix(c(1, 2), ncol = 2), 
    transition.array = matrix(c(1:4), ncol = 2))) # error because transition array is two-dimensional array
  expect_error(makeEnvironment(reward.matrix = matrix(c(1, NA), ncol = 2), 
    transition.array = array(1, dim = c(1, 1, 1)))) # error because of missing values in reward matrix
  expect_error(makeEnvironment(reward.matrix = matrix(c(1, 2), ncol = 2), 
    transition.array = array(1, dim = c(2, 1, 1)))) # ?
})
