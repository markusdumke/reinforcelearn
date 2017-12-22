context("getValueFunction")
val.fun = makeValueFunction("table", n.states = 8L, n.actions = 4L)
agent = makeAgent("random", val.fun)

test_that("getValueFunction returns action value function", {
  expect_equal(getValueFunction(agent), matrix(0, nrow = 8, ncol = 4))
})

test_that("getStateValues returns row max of action value function", {
  expect_equal(getStateValues(matrix(c(1, 2, 3, 4), ncol = 2)), c(3, 4))
})

set.seed(1)
context("getReplayMemory")
env = makeEnvironment("windy.gridworld")
memory = makeReplayMemory(size = 2L, batch.size = 2L)
agent = makeAgent("random", replay.memory = memory)
interact(env, agent, n.steps = 2L)

test_that("getReplayMemory returns list", {
  expect_equal(typeof(getReplayMemory(agent)), "list")
  expect_equal(getReplayMemory(agent), list(list(state = 30, action = 1,
    reward = -1, next.state = 31), list(state = 31, action = 2, reward = -1,
      next.state = 21)))
})

context("getEligibilityTraces")
