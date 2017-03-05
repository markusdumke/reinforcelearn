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

# test_that("TD Prediction works for gridworld", {
#   v = TD(random.policy, grid, n = 2, n.episodes = 10000, alpha = 0.01)
#   expect_equal(v, v.expected, tolerance = 0.1)
#   # what if n > length(episode)?
#   # v = TD(random.policy, grid, n = 100, n.episodes = 10000, alpha = 0.01)
#   # expect_equal(v, v.expected, tolerance = 0.1)
# })

grid$setEpisodeOverFalse()
left = c(1, 0, 0, 0)
right = c(0, 1, 0, 0)
up = c(0, 0, 1, 0)
down = c(0, 0, 0, 1)
# there are more than one optimal policy because of symmetry
optimal.policy = matrix(c(left, left, left, left, 
  up, up, right, down, 
  up, up, right, down, 
  up, right, right, left), ncol = 4, byrow = TRUE)
# test only states for which optimal policy is distinct
test_states = c(2, 3, 5, 8, 9, 12, 14, 15)

test_that("SARSA(0) converges to correct policy for gridworld", {
  Q = sarsa(grid, n.episodes = 1000)
  policy = make_greedy_policy(Q)
  # expect_equal(Q, Q.expected)
  expect_equal(policy[test_states, ], optimal.policy[test_states, ])
})

grid$setEpisodeOverFalse()
test_that("Q-Learning converges to correct policy for gridworld", {
  Q = qlearning(grid, n.episodes = 1000)
  policy = make_greedy_policy(Q)
  # expect_equal(Q, Q.expected)
  expect_equal(policy[test_states, ], optimal.policy[test_states, ])
})

# grid$setEpisodeOverFalse()
# test_that("Expected Sarsa converges to correct policy for gridworld", {
#   Q = expectedSarsa(grid, n.episodes = 1000)
#   policy = make_greedy_policy(Q)
#   # expect_equal(Q, Q.expected)
#   expect_equal(policy[test_states, ], optimal.policy[test_states, ])
# })

test_that("Sample epsilon-greedy action converges to correct probabilities", {
  set.seed(154)
  x = c()
  for (i in 1:10000){
    x = c(x, sample_epsilon_greedy_action(c(1, 2, 3), epsilon = 0.3))
  }
  expect_equal(as.data.frame(table(x))[, 2] / 10000, c(0.1, 0.1, 0.8), tol = 0.1)
})

test_that("epsilon-greedy policy works", {
  pi = make_epsilon_greedy_policy(matrix(c(2, 8, 4, 6), ncol = 2), epsilon = 0.2)
  expect_equal(pi, matrix(c(0.1, 0.9, 0.9, 0.1), ncol = 2))
})
