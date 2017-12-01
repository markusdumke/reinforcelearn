library(reinforcelearn)
context("QSigma")

test_that("Test that getPolicy creates epsilon-greedy policy", {
  expect_equal(getPolicy(c(0, 1, 2), epsilon = 0), matrix(c(0, 0, 1), ncol = 3))
  expect_equal(getPolicy(c(0, 0), epsilon = 0), matrix(c(1, 0), ncol = 2))
  expect_equal(getPolicy(matrix(c(0.5, 4), ncol = 2), epsilon = 0), matrix(c(0, 1), ncol = 2))
  expect_equal(getPolicy(c(10, 0), epsilon = 0.2), matrix(c(0.9, 0.1), ncol = 2))
  expect_equal(getPolicy(c(1, 0, 0), epsilon = 0.3), matrix(c(0.8, 0.1, 0.1), ncol = 3))
  expect_equal(getPolicy(1, epsilon = 0.5), matrix(1, ncol = 1))
  expect_equal(getPolicy(1, epsilon = 0), matrix(1, ncol = 1))
})

set.seed(1)
test_that("Test sampleActionFromPolicy", {
  expect_equal(sampleActionFromPolicy(1), 0)
  expect_equal(sampleActionFromPolicy(c(0.5, 0.5)), 1)
  expect_equal(sampleActionFromPolicy(c(0, 0, 1)), 2)
  expect_equal(sampleActionFromPolicy(c(0.9, 0.1)), 1)
  expect_equal(sampleActionFromPolicy(c(0.9, 0.1)), 0)
  expect_error(sampleActionFromPolicy(matrix(0, 0, 1)))
})

env = smallGridworld()
x = initializeReplayMemory(env, len = 1, identity)
x2 = initializeReplayMemory(env, len = 1, preprocessState = function(x) 100)
test_that("initializeReplayMemory works", {
  expect_equal(length(x), 1)
  expect_equal(class(x), "list")
  expect_equal(length(x[[1]]), 4)
  expect_equal(names(x[[1]]), c("state", "action", "reward", "next.state"))
  expect_equal(x2[[1]]$state, 100)
  expect_equal(x2[[1]]$next.state, 100)
})

# Create an environment from a transition array and reward matrix.
P = array(0, c(2, 2, 2))
P[, , 1] = matrix(c(0.5, 0.5, 0, 1), 2, 2, byrow = TRUE)
P[, , 2] = matrix(c(0, 1, 0, 1), 2, 2, byrow = TRUE)
R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)
env = makeEnvironment(transitions = P, rewards = R)
agent = qSigmaAgent$new(env, fun.approx = "table", preprocessState = identity,
  model = NULL, initial.value = NULL, n.states = NULL, n.episodes = 100, sigma = 1,
  target.policy = "greedy", lambda = 0, eligibility.type = 0, learning.rate = 0.1,
  epsilon = 0.1, discount = 1, double.learning = FALSE,
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01,
  updateEpsilon = identity2, updateSigma = identity2, updateLambda = identity2,
  updateAlpha = identity2, updateLearningRate = identity2)

agent$Q1 = matrix(1:4, ncol = 2)

test_that("predictQ works for table", {
  expect_equal(agent$predictQ(agent$Q1, 0), matrix(c(1, 3), ncol = 2))
  expect_equal(agent$predictQ(agent$Q1, 1), matrix(c(2, 4), ncol = 2))
})

test_that("epsilon of target policy is 0 for greedy policy", {
  expect_equal(agent$epsilon.target, 0)
})

Q = matrix(c(5, 6, 7, 8), ncol = 2)

decayParam = function(x, i) {
  if (i %% 1 == 0) {
    x = x * 0.5
  }
  x
}

prepState = function(x) {
  66
}

memory = vector("list", 2)
env$reset()
for (i in seq_along(memory)) {
  action = sample(env$actions, 1)
  env$step(1)
  memory[[i]] = list(state = env$previous.state, action = action,
    reward = env$reward, next.state = env$state)
}

agent = qSigmaAgent$new(env, fun.approx = "table", preprocessState = prepState,
  model = NULL, initial.value = Q, n.states = NULL, n.episodes = 100, sigma = 1,
  target.policy = "egreedy", lambda = 0.4, eligibility.type = 0, learning.rate = 0.1,
  epsilon = 0.1, discount = 1, double.learning = FALSE,
  replay.memory = memory, replay.memory.size = 1, batch.size = 1, alpha = 0.8, theta = 0.01,
  updateEpsilon = decayParam, updateSigma = decayParam, updateLambda = decayParam,
  updateAlpha = decayParam, updateLearningRate = decayParam)

test_that("initial value works", {
  expect_equal(agent$Q1, Q)
})

test_that("epsilon of target policy is equal to epsilon for egreedy policy", {
  expect_equal(agent$epsilon.target, 0.1)
})

env$reset()
test_that("parameters are updated if done is called", {
  expect_message(agent$done(env, 1), "Episode 1 finished after 0 steps with a return of 0")
  expect_equal(agent$episode.steps[1], 0)
  expect_equal(agent$epsilon, 0.1 * 0.5)
  expect_equal(agent$sigma, 1 * 0.5)
  expect_equal(agent$learning.rate, 0.1 * 0.5)
  expect_equal(agent$alpha, 0.8 * 0.5)
  expect_equal(agent$lambda, 0.4 * 0.5)
})

test_that("preprocessState is initialized correctly", {
  expect_equal(agent$preprocessState(2), 66)
})

test_that("replay memory argument is used", {
  expect_equal(agent$replay.memory, memory)
})

test_that("add2ReplayMemory works", {
  expect_equal(agent$replay.index, 0)
  # replace oldest enttry in replay memory
  expect_equal(agent$add2ReplayMemory(env, action = 13)[[1]],
    list(state = agent$preprocessState(env$previous.state), action = 13L,
      reward = env$reward, next.state = agent$preprocessState(env$state)))
  expect_equal(agent$replay.index, 1)
  agent$add2ReplayMemory(env, action = 7L)
  expect_equal(agent$replay.index, 2)
  agent$add2ReplayMemory(env, action = 4L)
  # replay memory will be set to 1 if end of replay memory is reached
  expect_equal(agent$replay.index, 1)
})

# fixme test sampleBatch with batch.size > 1
test_that("sampleBatch works", {
  expect_equal(agent$sampleBatch(), agent$replay.memory[[2]])
})

test_that("Prioritized experience replay works", {
  expect_equal(agent$priority, c(1, 1))
  expect_equal(agent$getIndices(replay.memory.size = 2, batch.size = 1), 1)
  agent$updatePriority(td.error = 100, theta = 0) # self$indices = 2
  expect_equal(agent$priority, c(1, 100))
})

agent = qSigmaAgent$new(env, fun.approx = "table", preprocessState = identity,
  model = NULL, initial.value = NULL, n.states = NULL, n.episodes = 100, sigma = 1,
  target.policy = "egreedy", lambda = 0, eligibility.type = 0, learning.rate = 0.1,
  epsilon = 0.1, discount = 1, double.learning = FALSE,
  replay.memory = memory, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01,
  updateEpsilon = identity2, updateSigma = identity2, updateLambda = identity2,
  updateAlpha = identity2, updateLearningRate = identity2)

test_that("unprioritized experience replay works", {
  expect_equal(agent$getIndices(replay.memory.size = 2, batch.size = 1), 2)
  expect_equal(agent$updatePriority(0, 0), NULL)
})

# to see if it converges, test with n.episodes = 500,
# then episodes should finish close the optimal solution of -4
env = gridworld(shape = c(3, 3), goal.states = 8, initial.state = 0)
test_that("runEpisode works without errors for tabular value function", {
  expect_error(qSigma(env, n.episodes = 2), NA)
  expect_error(qSigma(env, n.episodes = 2, double.learning = TRUE), NA)
  expect_error(qSigma(env, n.episodes = 2, lambda = 0.7), NA)
  expect_error(qSigma(env, n.episodes = 2, lambda = 0.7, double.learning = TRUE), NA)
  expect_error(qSigma(env, n.episodes = 2, replay.memory.size = 10, batch.size = 5), NA)
  expect_error(qSigma(env, n.episodes = 2, replay.memory.size = 10,
    batch.size = 5, double.learning = TRUE), NA)
})

oneHot = function(state) {
  one.hot = matrix(rep(0, env$n.states), nrow = 1)
  one.hot[1, state + 1] = 1
  one.hot
}

test_that("runEpisode works without errors for linear value function", {
  expect_error(qSigma(env, n.episodes = 2, fun.approx = "linear",
    preprocessState = oneHot), NA)
  expect_error(qSigma(env, n.episodes = 2, fun.approx = "linear",
    preprocessState = oneHot, double.learning = TRUE), NA)
  expect_error(qSigma(env, n.episodes = 2, fun.approx = "linear",
    preprocessState = oneHot, lambda = 0.7), NA)
  expect_error(qSigma(env, n.episodes = 2, fun.approx = "linear",
    preprocessState = oneHot, lambda = 0.7, double.learning = TRUE), NA)
  expect_error(qSigma(env, n.episodes = 2, fun.approx = "linear",
    preprocessState = oneHot, replay.memory.size = 10, batch.size = 5), NA)
  expect_error(qSigma(env, n.episodes = 2, fun.approx = "linear", double.learning = TRUE,
    preprocessState = oneHot, replay.memory.size = 10, batch.size = 5), NA)
})

if (requireNamespace("keras", quietly = TRUE)) {
  library(keras)
  model = keras_model_sequential()
  model %>% layer_dense(units = 4, activation = 'linear', input_shape = c(env$n.states))

  test_that("runEpisode works without errors for neural network value function", {
    expect_error(qSigma(env, n.episodes = 2, fun.approx = "neural.network",
      model = model, preprocessState = oneHot), NA)
  })
}

test_that("qSigma returns action value function, steps and returns per episode", {
  res = qSigma(env, n.episodes = 2)
  expect_equal(class(res), "list")
  expect_equal(dim(res$Q1), c(env$n.states, env$n.actions))
  expect_equal(length(res$steps), 2)
  expect_equal(length(res$returns), 2)
})
