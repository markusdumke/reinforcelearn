## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(message = FALSE, eval = TRUE, collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(reinforcelearn)
set.seed(123)

## ------------------------------------------------------------------------
# Gridworld environment
env = gridworld(shape = c(4, 4), goal.states = 15, initial.state = 0)

res = qlearning(env, n.episodes = 20)
# Note: to find a good policy we need to run more episodes.

## ------------------------------------------------------------------------
print(round(res$Q1, 2))

## ------------------------------------------------------------------------
# Values of each grid cell
state.values = matrix(apply(res$Q1, 1, max), ncol = 4, byrow = TRUE)
print(round(state.values, 1))

# Policy: Subtract 1 to be consistent with action numeration in env
policy = max.col(res$Q1) - 1
print(matrix(policy, ncol = 4, byrow = TRUE))

## ------------------------------------------------------------------------
print(res$steps)

## ------------------------------------------------------------------------
# This is equivalent to qlearning(env):
res = expectedSarsa(env, target.policy = "greedy", n.episodes = 20)

# Expected Sarsa with an epsilon-greedy target policy:
res = expectedSarsa(env, target.policy = "egreedy", n.episodes = 20)

## ------------------------------------------------------------------------
res = qSigma(env, sigma = 0.5, n.episodes = 20)

# This is equivalent to Sarsa:
res = qSigma(env, sigma = 1, n.episodes = 20)

# This is equivalent to Q-Learning:
res = qSigma(env, sigma = 0, target.policy = "greedy", n.episodes = 20)

## ------------------------------------------------------------------------
res = qlearning(env, epsilon = 0.2, learning.rate = 0.5, 
  discount = 0.99, n.episodes = 20)

# Decay epsilon over time. Every 10 episodes epsilon will be halfed.
decayEpsilon = function(epsilon, i) {
  if (i %% 10 == 0) {
    epsilon = epsilon * 0.5
  }
  epsilon
}

res = qlearning(env, epsilon = 0.5, n.episodes = 20,
  updateEpsilon = decayEpsilon)

## ------------------------------------------------------------------------
Q = matrix(100, nrow = env$n.states, ncol = env$n.actions)
res = qlearning(env, n.episodes = 5, initial.value = Q)

# After 5 episodes the Q values will still be similar to 100.
print(matrix(round(apply(res$Q1, 1, max), 1), ncol = 4, byrow = TRUE))

## ------------------------------------------------------------------------
env = mountainCar()
print(env$state.space)
print(env$state.space.bounds)

env$reset()
print(env$state)

## ---- out.width = "300px", fig.align="center", echo = FALSE--------------
knitr::include_graphics("gridtiling.JPG")

## ------------------------------------------------------------------------
# Define preprocessing function (we use grid tiling)
n.tilings = 8
max.size = 4096
iht = IHT(max.size)

position.max = env$state.space.bounds[[1]][2]
position.min = env$state.space.bounds[[1]][1]
velocity.max = env$state.space.bounds[[2]][2]
velocity.min = env$state.space.bounds[[2]][1]
position.scale = n.tilings / (position.max - position.min)
velocity.scale = n.tilings / (velocity.max - velocity.min)

# Scale state first, then get active tiles and return n hot vector
gridTiling = function(state) {
  state = c(position.scale * state[1], velocity.scale * state[2])
  active.tiles = tiles(iht, 8, state)
  nHot(active.tiles, max.size, out = "vector")
}

## ------------------------------------------------------------------------
res = qlearning(env, fun.approx = "linear", 
  preprocessState = gridTiling, n.episodes = 20)
print(res$steps)

## ---- eval = FALSE-------------------------------------------------------
#  env = gridworld(c(4, 4), goal.states = 15, initial.state = 0)
#  
#  # A one-hot feature vector
#  oneHot = function(state) {
#    one.hot = matrix(rep(0, env$n.states), nrow = 1)
#    one.hot[1, state + 1] = 1
#    one.hot
#  }
#  
#  # Define keras model
#  library(keras)
#  model = keras_model_sequential()
#  model %>% layer_dense(units = env$n.actions, activation = 'linear',
#    input_shape = c(env$n.states))
#  
#  res = qSigma(env, fun.approx = "neural.network", model = model,
#    preprocessState = oneHot, n.episodes = 20)

## ------------------------------------------------------------------------
env = gridworld(c(4, 4), goal.states = 15, initial.state = 0)

# Sarsa with replacing traces
res = sarsa(env, lambda = 0.9, eligibility.type = 1, n.episodes = 20)
print(res$steps)

## ------------------------------------------------------------------------
res = expectedSarsa(env, double.learning = TRUE, n.episodes = 20)
print(res$steps)

## ------------------------------------------------------------------------
# Fill a replay memory of size 100 on the gridworld task.
memory = vector("list", length = 100)
env$reset()
for (i in 1:100) {
  if (env$done) {
    env$reset()
  }
  action = sample(env$actions, size = 1)
  env$step(action)
  memory[[i]] = list(state = env$previous.state, action = action, 
    reward = env$reward, next.state = env$state)
}
print(memory[[1]])

# Pass on replay memory.
res = sarsa(env, replay.memory = memory, batch.size = 32, n.episodes = 20)

# Specify replay memory size, replay memory will be filled internally.
res = sarsa(env, replay.memory.size = 100, batch.size = 32, n.episodes = 20)
print(res$steps)

## ------------------------------------------------------------------------
# Prioritized experience replay
res = sarsa(env, replay.memory.size = 100, batch.size = 32, 
  n.episodes = 20, alpha = 0.5, theta = 0.01)
print(res$steps)

## ------------------------------------------------------------------------
# Random Walk Task (Sutton & Barto Example 6.2)
P = array(dim = c(7, 7, 2))
P[, , 1] = matrix(c(rep(c(1, rep(0, 6)), 2), c(0, 1, rep(0, 5)), 
  c(0, 0, 1, rep(0, 4)), c(rep(0, 3), 1, rep(0, 3)), c(rep(0, 4), 1, rep(0, 2)), 
  c(rep(0, 6), 1)), ncol = 7, byrow = TRUE)
P[, , 2] = matrix(c(c(1, rep(0, 6)), c(0, 0, 1, rep(0, 4)), 
  c(rep(0, 3), 1, rep(0, 3)), c(rep(0, 4), 1, rep(0, 2)), 
  c(rep(0, 5), 1, 0), c(rep(0, 6), 1), c(rep(0, 6), 1)), ncol = 7, byrow = TRUE)
R = matrix(c(rep(0, 12), 1, 0), ncol = 2)
env = makeEnvironment(transitions = P, rewards = R, initial.state = 3)

# Uniform random policy
random.policy = matrix(1 / env$n.actions, nrow = env$n.states, 
  ncol = env$n.actions)

# Estimate state value function with TD(0)
res = td(env, random.policy, n.episodes = 20, lambda = 0.5)
print(res$V)

## ------------------------------------------------------------------------
# Set up gridworld problem
env = smallGridworld()
  
# Define uniform random policy, take each action with equal probability
random.policy = matrix(1 / env$n.actions, nrow = env$n.states, 
  ncol = env$n.actions)

# Evaluate this policy
res = evaluatePolicy(env, random.policy, precision = 0.01)
print(round(matrix(res$v, ncol = 4, byrow = TRUE)))

## ------------------------------------------------------------------------
# Find optimal policy using Policy Iteration
res = iteratePolicy(env)
print(round(matrix(res$v, ncol = 4, byrow = TRUE)))

## ------------------------------------------------------------------------
# Find optimal policy using Value Iteration
res = iterateValue(env, n.iter = 100)
print(round(matrix(res$v, ncol = 4, byrow = TRUE)))

## ------------------------------------------------------------------------
env = mountainCar()

# Linear function approximation and softmax policy
res = actorCritic(env, fun.approx = "linear", 
  preprocessState = gridTiling, n.episodes = 20)
print(res$steps)

## ------------------------------------------------------------------------
# Mountain Car with continuous action space
env = mountainCar(action.space = "Continuous")

# Linear function approximation and gaussian policy
set.seed(123)
res = actorCritic(env, fun.approx = "linear", policy = "gaussian", 
  preprocessState = gridTiling, n.episodes = 20)
print(res$steps)

## ------------------------------------------------------------------------
# Cliff walking environment
rewardFun = function(state, action, n.state) {
  if (n.state %in% 37:46) {
    return(- 100)
  } else {
    return(- 1)
  }
}
env = gridworld(shape = c(4, 12), goal.states = 47,
  cliff.states = 37:46, reward.step = - 1, reward.cliff = - 100,
  cliff.transition.done = TRUE, initial.state = 36, sampleReward = rewardFun)

res = actorCritic(env, n.episodes = 20, lambda.actor = 0.5, lambda.critic = 0.8)

## ------------------------------------------------------------------------
# Define reward function
rewardFun = function(action) {
  if (action == 0) {
    reward = rnorm(1, mean = 1, sd = 1)
  }
  if (action == 1) {
    reward = rnorm(1, mean = 2, sd = 4)
  }
  if (action == 2) {
    reward = runif(1, min = 0, max = 5)
  }
  if (action == 3) {
    reward = rexp(1, rate = 0.25)
  }
  reward
}

## ------------------------------------------------------------------------
# Greedy action selection.
bandit(rewardFun, n.actions = 4, n.episodes = 1000,
  action.selection = "greedy")

# Epsilon-greedy action selection.
bandit(rewardFun, n.actions = 4, n.episodes = 1000, 
  action.selection = "egreedy", epsilon = 0.2)

# Upper-confidence bound action selection.
bandit(rewardFun, n.actions = 4, n.episodes = 1000, 
  action.selection = "UCB", C = 2)

# Gradient-bandit algorithm.
bandit(rewardFun, n.actions = 4, n.episodes = 10000, 
  action.selection = "gradientbandit", alpha = 0.1)

## ------------------------------------------------------------------------
# Greedy action selection with optimistic initial values.
bandit(rewardFun, n.actions = 4, n.episodes = 1000, 
  action.selection = "greedy", 
  initial.value = 5, initial.visits = 100)

