## ------------------------------------------------------------------------
library(reinforcelearn)
grid = WindyGridworld$new()
WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array, 
  reward.matrix = grid$reward.matrix, 
  terminal.states = grid$terminal.states, 
  initial.state = grid$initial.state)

## ---- results = "hide"---------------------------------------------------
res = sarsa(WindyGridworld1, lambda = 0, alpha = 0.5, epsilon = 0.1, n.steps = 8000)
# res = qlearning(WindyGridworld1, n.episodes = 100000)
Q = res$Q

## ------------------------------------------------------------------------
print(matrix(apply(res$Q, 1, max), ncol = 10, byrow = TRUE))
# Greedy policy with respect to Q
optimal.policy = max.col(Q)
print(matrix(optimal.policy, ncol = 10, byrow = TRUE))

## ------------------------------------------------------------------------
plot(x = res$time.steps.episode, y = seq_along(res$time.steps.episode),
 type = "l", xlim = c(0, 8000), ylab = "Episode", xlab = "Time steps",
 main = "Episodes completed per time step")
plot(x = seq_along(res$time.steps.episode), y = diff(c(0, res$time.steps.episode)),
 type = "l", xlim = c(0, length(res$time.steps.episode)), ylab = "Episode length", xlab = "Episode", main = "Episode length over time")

