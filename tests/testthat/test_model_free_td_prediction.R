library(reinforcelearn)
context("Model-free TD Prediction")

set.seed(1477)
grid = gridworld$new()

# Define random policy
n.states = nrow(grid$reward.matrix)
n.actions = ncol(grid$reward.matrix)
random.policy = matrix(1 / n.actions, nrow = n.states, ncol = n.actions)

# Compare results with expected result
v.expected = c(0, -14, -20, -22, -14, -18, -20, -20,
  -20, -20, -18, -14, -22, -20, -14, 0)

# test_that("Test that TD Prediction works for gridworld", {
#   v = TD(random.policy, grid, n = 2, n.episodes = 10000, alpha = 0.01)
#   expect_equal(v, v.expected, tolerance = 0.1)
#   # what if n > length(episode)?
#   # v = TD(random.policy, grid, n = 100, n.episodes = 10000, alpha = 0.01)
#   # expect_equal(v, v.expected, tolerance = 0.1)
# })
