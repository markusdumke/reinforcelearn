# library(reinforcelearn)
# context("Evaluate Policy")
# 
# # Define uniform random policy, take each action with probability 0.25
# grid = makeGridworld()
# n.states = nrow(grid$reward.matrix)
# n.actions = ncol(grid$reward.matrix)
# random.policy = matrix(1 / n.actions, nrow = n.states, ncol = n.actions)
# 
# v.expected = c(0, -14, -20, -22, -14, -18, -20, -20,
#   -20, -20, -18, -14, -22, -20, -14, 0)
# 
# test_that("Test that evaluate Policy works for gridworlds of different shapes", {
#   v = evaluatePolicy(random.policy, grid)
#   expect_equal(v, v.expected, tolerance = 0.1)
# })
