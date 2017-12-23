context("environment creation")

env1 = makeEnvironment("mountain.car",
  action.names = c("stop" = 0L, "pause" = 1L, "go" = 2L))
env2 = makeEnvironment("windy.gridworld", discount = 0.8)
env3 = makeEnvironment("gridworld", shape = c(2, 3), initial.state = 5L,
  goal.states = 0L, discount = 0.9, diagonal.moves = TRUE)

context("discount")
test_that("discount will be initialized correctly", {
  expect_equal(env1$discount, 1)
  expect_equal(env2$discount, 0.8)
  expect_equal(env3$discount, 0.9)
})

context("action.names")
test_that("action names will be initialized correctly", {
  expect_equal(env1$action.names, c("stop" = 0L, "pause" = 1L, "go" = 2L))
  expect_equal(env2$action.names, c("left" = 0L, "right" = 1L, "up" = 2L, "down" = 3L))
  expect_equal(env3$action.names, c("left" = 0L, "right" = 1L, "up" = 2L, "down" = 3L,
    "leftup" = 4L, "leftdown" = 5L, "rightup" = 6L, "rightdown" = 7L))
})

env4 = makeEnvironment("windy.gridworld")
env5 = makeEnvironment("windy.gridworld")
test_that("action.names are equivalent to integer actions", {
  env4$step("left")
  env5$step(0L)
  expect_equal(env4, env5)
})

context("visualization")
test_that("gridworld visualization works", {
  expect_equal(visualizeGridworld(c(2, 2), current.state = 3L),
    paste0(" - ", "- ", "\n", " - ", "o"))
})

context("counter of steps, returns")
env3$step(0L)
test_that("env$episode.return computes discounted reward sum", {
  expect_equal(env3$episode.return, -1)
  env3$step(0L)
  expect_equal(env3$episode.return, -1 + env3$discount * env3$reward)
})
