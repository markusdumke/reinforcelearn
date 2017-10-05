library(reinforcelearn)
context("Dynamic Programming")

P = array(c(1, 0.5, 0, 0.5, 1, 0.2, 0, 0.8, 1, 0.4, 0, 0.6), dim = c(2, 2, 3))
R = matrix(0, nrow = 2, ncol = 3)
gamma = 1
v = c(0, 1)
res = getActionValue(P, R, gamma, v)

test_that("getActionValue does the right thing", {
  expect_equal(class(res), "matrix")
  expect_equal(nrow(res), 2)
  expect_equal(ncol(res), 3)
  expect_equal(res, matrix(c(0, 0.5, 0, 0.8, 0, 0.6), nrow = 2, ncol = 3))
})

grid = gridworld()

random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
  ncol = grid$n.actions)

res = evaluatePolicy(grid, random.policy, precision = 0.01)
res2 = evaluatePolicy(grid, random.policy, n.iter = 1000)

test_that("evaluatePolicy works for gridworld", {
  expect_equal(class(res), "list")
  expect_equal(length(res), 3)
  expect_equal(res$policy, random.policy)
  expect_equal(round(res$v), c(0, - 14, - 20, - 22, - 14, - 18, - 20, 
    - 20, - 20, - 20, - 18, - 14, - 22, - 20, - 14, 0))
  expect_equal(round(res2$v), c(0, - 14, - 20, - 22, - 14, - 18, - 20, 
    - 20, - 20, - 20, - 18, - 14, - 22, - 20, - 14, 0))
})

res = iteratePolicy(grid, precision.eval = 0.1)
res2 = iteratePolicy(grid, n.iter = 1000)

test_that("iteratePolicy works for gridworld", {
  expect_equal(class(res), "list")
  expect_equal(length(res), 3)
  expect_equal(round(matrix(res$v, ncol = 4, byrow = TRUE)), 
    matrix(c(0, - 1, - 2, - 3, - 1, - 2, - 3, - 2, - 2, - 3, 
      - 2, - 1, - 3, - 2, - 1, 0), ncol = 4))
  expect_equal(round(matrix(res2$v, ncol = 4, byrow = TRUE)), 
    matrix(c(0, - 1, - 2, - 3, - 1, - 2, - 3, - 2, - 2, - 3, 
      - 2, - 1, - 3, - 2, - 1, 0), ncol = 4))
})

res = iterateValue(grid)
res2 = iterateValue(grid, n.iter = 1000)

test_that("iterateValue works for gridworld", {
  expect_equal(class(res), "list")
  expect_equal(length(res), 3)
  expect_equal(round(matrix(res$v, ncol = 4, byrow = TRUE)), 
    matrix(c(0, - 1, - 2, - 3, - 1, - 2, - 3, - 2, - 2, - 3, 
      - 2, - 1, - 3, - 2, - 1, 0), ncol = 4))
  expect_equal(round(matrix(res2$v, ncol = 4, byrow = TRUE)), 
    matrix(c(0, - 1, - 2, - 3, - 1, - 2, - 3, - 2, - 2, - 3, 
      - 2, - 1, - 3, - 2, - 1, 0), ncol = 4))
})

policy = matrix(1, nrow = 16, ncol = 4)
v = rep(1, 16)
q = matrix(1, nrow = 16, ncol = 4)

test_that("input checking works for dp", {
  expect_error(evaluatePolicy(grid, policy), 
    "The probabilities of each row of the policy must sum to 1.")
  expect_error(iteratePolicy(grid, policy), 
    "The probabilities of each row of the policy must sum to 1.")
  expect_error(evaluatePolicy(grid, random.policy, v), 
    "State values of terminal states must be 0!")
  expect_error(iterateValue(grid, v), 
    "State values of terminal states must be 0!")
  expect_error(iterateValue(grid, q = q), 
    "Action values of terminal states must be 0!")
  expect_error(evaluatePolicy(grid, random.policy, q = q), 
    "Action values of terminal states must be 0!")
})

Q = matrix(1:5, nrow = 1, ncol = 5)
Q2 = matrix(c(1, 1), nrow = 1, ncol = 2)
policy = improvePolicy(Q)
set.seed(123)
policy2 = improvePolicy(Q2)
policy2b = improvePolicy(Q2)

test_that("improvePolicy returns greedy policy", {
  expect_equal(class(policy), "matrix")
  expect_equal(policy, matrix(c(0, 0, 0, 0, 1), nrow = 1))
  expect_equal(policy2, matrix(c(1, 0), nrow = 1))
  expect_equal(policy2b, matrix(c(0, 1), nrow = 1))
})
