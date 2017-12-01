library(reinforcelearn)
context("Gridworlds")

grid = gridworld(shape = c(2, 2), goal.states = 0)

test_that("gridworld creates basic gridworlds", {
  expect_equal(class(grid), c("envir", "R6"))
  expect_equal(dim(grid$transitions), c(4, 4, 4))
  expect_equal(dim(grid$rewards), c(4, 4))
  expect_equal(grid$transitions[, , 1], matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 4, byrow = TRUE))
  expect_equal(grid$rewards, matrix(c(rep(0, 4), rep(-1, 12)), ncol = 4, byrow = TRUE))
})

test_that("gridworld works if there is only one row or column", {
  expect_error(gridworld(c(1, 2), goal.states = 1), NA)
  expect_error(gridworld(c(2, 1), goal.states = 1), NA)
  expect_error(gridworld(c(1, 1), goal.states = 0), "A gridworld with only one state is not allowed!")
})

test_that("input checking in gridworld works", {
  expect_error(gridworld(shape = c(2, 2), goal.states = 5),
    "All states must be inside the grid! States are numerated row-wise starting with 0, check Details!")
  expect_error(gridworld(shape = c(2, 2), goal.states = 2, cliff.states = 5),
    "All states must be inside the grid! States are numerated row-wise starting with 0, check Details!")
  expect_error(gridworld(shape = c(2, 2), goal.states = 1, cliff.transition.states = 10),
    "All states must be inside the grid! States are numerated row-wise starting with 0, check Details!")
  expect_error(gridworld(shape = c(4, 2, 3), goal.states = 1))
  expect_error(gridworld(shape = c(2, 2), goal.states = 1, wind = 0))
  expect_error(gridworld(shape = c(2, 2), goal.states = 1, stochasticity = 1.3))
})

wind = gridworld(shape = c(2, 2), goal.states = 3, wind = c(1, 0))

test_that("windy gridworlds can be created", {
  expect_equal(wind$transitions[3, , 2], c(0, 1, 0, 0))
})

cliff.walking = gridworld(shape = c(2, 2), goal.states = 3,
  cliff.transition.states = 0, cliff.states = 1, reward.cliff = -200)

test_that("reward matrix is correct", {
  expect_equal(cliff.walking$rewards[2, ], rep(- 200, 4))
  expect_equal(wind$rewards[4, ], rep(0, 4))
  expect_equal(cliff.walking$rewards[1, ], rep(- 1, 4))
})

stochastic.grid = gridworld(shape = c(4, 4), goal.states = 15, stochasticity = 0.2)

test_that("stochasticity is applied correctly", {
  expect_equal(stochastic.grid$transitions[11, c(6, 7, 8, 10, 12, 14, 16), 4], rep(0.025, 7))
  expect_equal(stochastic.grid$transitions[11, 15, 4], 0.8 + 0.2 / 8)
  expect_equal(stochastic.grid$transitions[3, c(2, 3, 4, 6, 7, 8), 2],
    c(0.05, 0.025, 0.85, 0.025, 0.025, 0.025))
})

# grid = gridworld(diagonal.moves = TRUE)
#
# test_that("diagonal moves produce the right transition probabilities", {
#
# })
