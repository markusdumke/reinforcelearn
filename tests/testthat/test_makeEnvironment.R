library(reinforcelearn)
context("makeEnvironment")

g = makeGridworld(c(1, 2), 1)

r = g$transitions - 7
m = makeEnvironment(transitions = g$transitions, rewards = r, initial.state = 0)
m$reset()
m$step(1)

test_that("rewards can be a matrix or an 3-dim. array", {
  expect_error(makeEnvironment(transitions = g$transitions, rewards = g$rewards), NA)
  expect_error(makeEnvironment(transitions = g$transitions, rewards = r), NA)
  expect_equal(m$reward, - 6)
})

sampleReward = function(state, action, n.state) {
  return(10)
}

grid = makeEnvironment(transitions = g$transitions, sampleReward = sampleReward)
grid$reset()
grid$step(1L)

test_that("reward function argument works", {
  expect_equal(grid$reward, 10)
})

m = makeEnvironment(transitions = g$transitions, rewards = g$rewards, initial.state = 1)
m$reset()

resetGrid = function() {
  p = c(1, 0)
  sample(0:1, prob = p, size = 1)
}
m2 = makeGridworld(c(1, 2), 1, reset = resetGrid)
m2$reset()

test_that("reset works", {
  expect_equal(m$state, 1)
  expect_equal(m2$state, 0)
})


