library(reinforcelearn)
context("Make Gridworld")

grid = makeGridworld(goal.states = c(1, 16))

test_that("makeGridworld creates basic gridworlds", {
  expect_equal(length(grid), 2)
  expect_equal(names(grid), c("transitions", "rewards"))
  expect_equal(typeof(grid), "list")
  expect_equal(dim(grid$transitions), c(16, 16, 4))
  expect_equal(dim(grid$rewards), c(16, 4))
  expect_equal(grid$transitions, gridworld$transitions)
  expect_equal(grid$rewards, gridworld$rewards)
})

test_that("makeGridworld works if there is only one row or column", {
  expect_error(makeGridworld(c(1, 2), goal.states = 1), NA)
  expect_error(makeGridworld(c(2, 1), goal.states = 1), NA)
  expect_error(makeGridworld(c(1, 1), goal.states = 1), NA)
})

test_that("input checking in makeGridworld works", {
  expect_error(makeGridworld(shape = c(2, 2), goal.states = 5), 
    "All states must be inside the grid! States are numerated row-wise starting with 1, check Details!")
  expect_error(makeGridworld(shape = c(2, 2), goal.states = 2, cliff.states = 5), 
    "All states must be inside the grid! States are numerated row-wise starting with 1, check Details!")
  expect_error(makeGridworld(shape = c(2, 2), goal.states = 1, cliff.transition.states = 10), 
    "All states must be inside the grid! States are numerated row-wise starting with 1, check Details!")
  expect_error(makeGridworld(shape = c(4, 2, 3), goal.states = 1))
  expect_error(makeGridworld(shape = c(2, 2), goal.states = 1, wind = 0))
  expect_error(makeGridworld(shape = c(2, 2), goal.states = 1, stochasticity = 1.3))
  expect_error(makeGridworld(shape = c(2, 2), goal.states = 1, cliff.states = 1), 
    "Please specify the cliff.transition.states!")
})

wind = makeGridworld(shape = c(2, 2), goal.states = 4, wind = c(1, 0))

test_that("windy gridworlds can be created", {
  expect_equal(wind$transitions[3, , 2], c(0, 1, 0, 0))
})

cliff.walking = makeGridworld(shape = c(2, 2), goal.states = 4, 
  cliff.transition.states = 1, cliff.states = 2, reward.cliff = -200)

test_that("reward matrix is correct", {
  expect_equal(cliff.walking$rewards[2, ], rep(- 200, 4))
  expect_equal(wind$rewards[4, ], rep(0, 4))
  expect_equal(cliff.walking$rewards[1, ], rep(- 1, 4))
})

stochastic.grid = makeGridworld(shape = c(4, 4), goal.states = 16, stochasticity = 0.2)

test_that("stochasticity is applied correctly", {
  expect_equal(stochastic.grid$transitions[11, c(6, 7, 8, 10, 12, 14, 16), 4], rep(0.025, 7))
  expect_equal(stochastic.grid$transitions[11, 15, 4], 0.8 + 0.2 / 8)
  expect_equal(stochastic.grid$transitions[3, c(2, 3, 4, 6, 7, 8), 2], 
    c(0.05, 0.025, 0.85, 0.025, 0.025, 0.025))
})

g = makeGridworld(shape = c(4, 12), goal.states = 48, cliff.states = 38:47, 
  reward.step = - 1, reward.cliff = - 100, cliff.transition.states = 37)

test_that("cliff walking gridworld can be created", {
  expect_equal(g$transitions, cliff$transitions)
  expect_equal(g$rewards, cliff$rewards)
})

g = makeGridworld(shape = c(7, 10), goal.states = 38, 
  reward.step = - 1, wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0))

test_that("windy gridworld can be created", {
  expect_equal(g$transitions, windy.gridworld$transitions)
  expect_equal(g$rewards, windy.gridworld$rewards)
})

# grid = makeGridworld(diagonal.moves = TRUE)
# 
# test_that("diagonal moves produce the right transition probabilities", {
# 
# })
