## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(message = FALSE, eval = T, collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(reinforcelearn)

## ------------------------------------------------------------------------
# Windy gridworld environment
env = windyGridworld()

res = qlearning(env, n.episodes = 50)
# Note: to find the optimal policy we need to run at least 500 episodes.

## ------------------------------------------------------------------------
print(res$steps)

## ------------------------------------------------------------------------
# Values of each grid cell
state.values = matrix(apply(res$Q1, 1, max), ncol = 10, byrow = TRUE)
print(round(state.values, 1))

# Subtract 1 to be consistent with action numeration in env
optimal.policy = max.col(res$Q1) - 1
print(matrix(optimal.policy, ncol = 10, byrow = TRUE))

## ------------------------------------------------------------------------
res = qlearning(env, epsilon = 0.2, learning.rate = 0.5, discount = 0.99)

# Decay epsilon over time. Every 10 episodes epsilon will be halfed.
decayEpsilon = function(epsilon, i) {
  if (i %% 10 == 0) {
    epsilon = epsilon * 0.5
  }
  epsilon
}

res = qlearning(env, epsilon = 0.5, updateEpsilon = decayEpsilon)

## ------------------------------------------------------------------------
Q = matrix(100, nrow = env$n.states, ncol = env$n.actions)
res = qlearning(env, n.episodes = 5, initial.value = Q)
# After only 5 episodes the Q values will still be similar to the initial values.
print(matrix(round(apply(res$Q1, 1, max), 1), ncol = 10, byrow = TRUE))

## ------------------------------------------------------------------------
env = MountainCar()
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
  makeNHot(active.tiles, max.size, out = "vector")
}

## ------------------------------------------------------------------------
set.seed(123)
res = qlearning(env, fun.approx = "linear", preprocessState = gridTiling, n.episodes = 20)
print(res$steps)

## ------------------------------------------------------------------------
env = windyGridworld()
res = sarsa(env, n.episodes = 50)
print(res$steps)

## ------------------------------------------------------------------------
# This is equivalent to qlearning(env):
res = expectedSarsa(env, target.policy = "greedy")

# With an epsilon-greedy target policy:
res = expectedSarsa(env, target.policy = "e-greedy")

## ------------------------------------------------------------------------
res = qSigma(env, sigma = 0.5)

# This is equivalent to Sarsa:
res = qSigma(env, sigma = 1)

# This is equivalent to Q-learning:
res = qSigma(env, sigma = 0, target.policy = "greedy")

## ------------------------------------------------------------------------
res = sarsa(env, lambda = 0.9, eligibility.type = 1, n.episodes = 50)
print(res$steps)

## ------------------------------------------------------------------------
res = expectedSarsa(env, double.learning = TRUE, n.episodes = 50)
print(res$steps)

## ------------------------------------------------------------------------
# Fill a replay memory of size 100 on the mountain car task.
# We will use grid tiling as defined above.
memory = vector("list", length = 100)
env = MountainCar()
env$reset()
for (i in 1:100) {
  if (env$done) {
    env$reset()
  }
  action = sample(0:2, size = 1, prob = c(0.5, 0, 0.5))
  env$step(action)
  memory[[i]] = list(state = gridTiling(env$previous.state), action = action, 
    reward = env$reward, next.state = gridTiling(env$state))
}

# res = sarsa(env, fun.approx = "linear", preprocessState = gridTiling, 
#   replay.memory = memory, batch.size = 32, n.episodes = 30)
# print(res$steps)

# res = sarsa(env, fun.approx = "linear", preprocessState = gridTiling, 
#   replay.memory.size = 100, batch.size = 32, n.episodes = 30)

## ------------------------------------------------------------------------
# Prioritized experience replay
# res = sarsa(env, fun.approx = "linear", preprocessState = gridTiling,
#   replay.memory.size = 100, batch.size = 32, n.episodes = 30,
#   alpha = 0.5, theta = 0.05)

