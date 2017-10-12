library(reinforcelearn)
context("QSigma")

# test in the following if no error occurs
# Fixme: we need more detailed tests here

grid = windyGridworld()

n = 2
test_that("tabular qsigma works", {
  expect_error(qSigma(grid, sigma = 0.5, n.episodes = n), NA)
  expect_error(qlearning(grid, n.episodes = n), NA)
  expect_error(sarsa(grid, n.episodes = n), NA)
})

test_that("tabular qsigma works with bootstrapping", {
  expect_error(qSigma(grid, sigma = 0.7, n.episodes = n, lambda = 0.9), NA)
})

test_that("tabular qsigma works with double learning", {
  expect_error(qSigma(grid, sigma = 0.2, n.episodes = n, double.learning = TRUE, 
    update.target.after = 2), NA)
})

# fixme
# test_that("tabular qsigma works with double learning and bootstrapping", {
#   expect_error(qSigma(grid, sigma = 0.8, n.episodes = n, double.learning = TRUE, 
#     update.target.after = 4, lambda = 0.7), NA)
# })


size = 100
memory = vector("list", length = size)
grid$reset()
action = sample(grid$actions, 1)
for (i in seq_len(size)) {
  grid$step(action)
  memory[[i]] = list(state = grid$previous.state, action = action, reward = grid$reward, 
    next.state = grid$state)
  action = sample(grid$actions, 1)
  if (grid$done) {
    grid$reset()
  }
}

test_that("tabular qsigma works with experience replay", {
  expect_error(qSigma(grid, sigma = 0.5, n.episodes = n, replay.memory.size = 10, batch.size = 5), NA)
  # with prefilled replay.memory #fixme
  # expect_error(qSigma(grid, sigma = 0.5, n.episodes = n, replay.memory = memory), NA)
})

test_that("tabular qsigma works with prioritized experience replay", {
  expect_error(qSigma(grid, sigma = 0.5, n.episodes = n, replay.memory.size = 10, 
    batch.size = 5, alpha = 0.7, eligibility.type = 0.05), NA)
})

# test neural network

# test update params
