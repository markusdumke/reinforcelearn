## ------------------------------------------------------------------------
library(reinforcelearn)

## ------------------------------------------------------------------------
t = array(c(0.5, 0, 0.5, 1, 0.2, 0, 0.8, 1), c(2, 2, 2))
r = matrix(c(- 1, 0, - 1, 0), ncol = 2)
Env = makeEnvironment(transition.array = t, reward.matrix = r)

## ---- eval = FALSE-------------------------------------------------------
#  MCar = makeEnvironment("MountainCar-v0")

## ---- eval = FALSE-------------------------------------------------------
#  res = sarsa(Env)

## ------------------------------------------------------------------------
library(reinforcelearn)
grid = WindyGridworld$new()
WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array, 
  reward.matrix = grid$reward.matrix, 
  initial.state = 30L)

## ---- results = "hide"---------------------------------------------------
res = sarsa(WindyGridworld1, n.episodes = 1000, seed = 123)

## ------------------------------------------------------------------------
# Optimal action value function
optimal.Q = apply(res$Q, 1, max)
print(matrix(optimal.Q, ncol = 10, byrow = TRUE))

# Optimal policy
optimal.policy = max.col(res$Q)
print(matrix(optimal.policy, ncol = 10, byrow = TRUE))

## ------------------------------------------------------------------------
plot(x = cumsum(res$steps.per.episode)[1:300], y = 1:300,
 type = "l", xlim = c(0, 20000), ylab = "Number of Episodes", xlab = "Time steps",
 main = "Episodes completed per time step")
plot(x = seq_along(res$steps.per.episode), y = res$steps.per.episode,
 type = "l", xlim = c(0, 1000), ylab = "Episode length", xlab = "Episode", main = "Episode length over time")

