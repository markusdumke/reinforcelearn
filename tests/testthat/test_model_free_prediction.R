library(reinforcelearn)
context("Model-free-Prediction")

set.seed(1477)
grid = gridworld_R6$new()

# Define random policy
n.states = nrow(grid$reward.matrix)
n.actions = ncol(grid$reward.matrix)
random.policy = matrix(1 / n.actions, nrow = n.states, ncol = n.actions)

# Compare results with expected result
v.expected = c(NA, -14, -20, -22, -14, -18, -20, -20,
  -20, -20, -18, -14, -22, -20, -14, NA)

test_that("Test that Monte Carlo first-visit Prediction works", {
  v = predictMC(random.policy, grid, n.episodes = 10000, method = "first-visit")
  expect_equal(v, v.expected, tolerance = 0.5)
})

test_that("Test that Monte Carlo every-visit Prediction works", {
  v = predictMC(random.policy, grid, n.episodes = 10000, method = "every-visit")
  expect_equal(v, v.expected, tolerance = 0.5)
})
