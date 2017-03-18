library(reinforcelearn)

context("Test that gridworld works")

test_that("Input checking works", {
  expect_error(gridworld$new(shape = c(2, 2), terminal.states = 8), 
    "Terminal states must be inside the grid!")
  # expect_error(gridworld$new(terminal.states = 0))
})

test_that("Initialization of attributes works", {
  grid = gridworld$new(shape = c(2, 2), terminal.states = 1)
  expect_equal(grid$n.states, 4)
  expect_equal(grid$terminal.states , 1)
  expect_equal(grid$non.terminal.states, c(2, 3, 4))
  expect_equal(grid$shape, c(2, 2))
  expect_equal(grid$states, seq(1, 4))
  expect_equal(grid$n.states, 4)
  expect_equal(grid$n.steps, 0)
  expect_equal(grid$n.actions, 4)
  expect_equal(grid$actions, c("left", "right", "up", "down"))
  expect_equal(grid$episode.over, FALSE)
})

test_that("Reward matrix is correct", {
  grid = gridworld$new(shape = c(2, 1), terminal.states = 1)
  res = matrix(rep(c(0, - 1), 4), ncol = 4)
  colnames(res) <- c("left", "right", "up", "down")
  expect_equal(grid$reward.matrix, res)
})


test_that("Transition array is correct", {
  grid = gridworld$new(shape = c(2, 1), terminal.states = 1)
  expect_equal(dim(grid$transition.array), c(2, 2, 4))
})
