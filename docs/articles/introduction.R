## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(message = FALSE, eval = TRUE, collapse = TRUE, comment = "#>")

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

## ------------------------------------------------------------------------
# Show value of each state.
print(matrix(round(apply(res$Q1, 1, max), 1), ncol = 10, byrow = TRUE))

## ------------------------------------------------------------------------
policy = max.col(res$Q1) - 1L
print(matrix(policy, ncol = 10, byrow = TRUE))

