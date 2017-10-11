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

print("Reset the environment")
env$reset()
paste("The state is initially", env$state)
print("Take action 1")
env$step(1)
paste("The state is now", env$state)
paste("And the reward for action 1 was", env$reward)

## ------------------------------------------------------------------------
# Solve the windy gridworld task using Q-Learning
# env = windyGridworld()
# res = qlearning(env)

