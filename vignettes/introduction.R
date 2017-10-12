## ------------------------------------------------------------------------
library(reinforcelearn)

## ------------------------------------------------------------------------
P = array(0, c(2, 2, 2))
P[, , 1] = matrix(c(0.5, 0.5, 0, 1), 2, 2, byrow = TRUE)
P[, , 2] = matrix(c(0, 1, 0, 1), 2, 2, byrow = TRUE)
R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)  
env = makeEnvironment(transitions = P, rewards = R)

## ------------------------------------------------------------------------
paste("State space:" , env$state.space)
paste("Number of actions:", env$n.actions)

# Reset the environment
env$reset()
print(env)

env$step(1)
print(env)

## ------------------------------------------------------------------------
# Solve the windy gridworld task using Q-Learning
env = windyGridworld()
res = qlearning(env)
res$steps

