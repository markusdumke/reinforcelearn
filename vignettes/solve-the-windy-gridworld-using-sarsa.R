## ------------------------------------------------------------------------
library(reinforcelearn)
grid = WindyGridworld$new()
WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array, 
  reward.matrix = grid$reward.matrix, 
  terminal.states = grid$terminal.states, 
  initial.state = 30)

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

