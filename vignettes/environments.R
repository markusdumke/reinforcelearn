## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(message = FALSE, eval = TRUE, collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(reinforcelearn)
set.seed(1)

## ---- eval = FALSE-------------------------------------------------------
#  # Create an OpenAI Gym environment.
#  # Make sure you have Python and Gym installed.
#  # Start server from within R.
#  package.path = system.file(package = "reinforcelearn")
#  path2pythonfile = paste0(package.path, "/gym_http_server.py")
#  system2("python", args = path2pythonfile, stdout = NULL,
#    wait = FALSE, invisible = FALSE)
#  
#  env = makeEnvironment("MountainCar-v0")

## ---- eval = FALSE-------------------------------------------------------
#  env$reset()
#  for (i in 1:200) {
#    action = sample(env$actions, 1)
#    env$step(action)
#  }
#  env$close()

## ---- out.width = "300px", fig.align="center", echo = FALSE--------------
knitr::include_graphics("mountaincar.JPG")

## ------------------------------------------------------------------------
# State transition array
P = array(0, c(2, 2, 2))
P[, , 1] = matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow = TRUE)
P[, , 2] = matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow = TRUE)
print(P)
# Reward matrix
R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)
print(R)
env = makeEnvironment("MDP", transitions = P, rewards = R)

## ------------------------------------------------------------------------
P = array(0, c(2, 2, 2))
P[, , 1] = matrix(c(0.5, 0.5, 0, 1), 2, 2, byrow = TRUE)
P[, , 2] = matrix(c(0.1, 0.9, 0, 1), 2, 2, byrow = TRUE)
print(P)

env = makeEnvironment("MDP", transitions = P, rewards = R)
print(env$terminal.states)

## ------------------------------------------------------------------------
env = makeEnvironment("MDP", transitions = P, rewards = R, initial.state = 0L)
env$reset()

## ---- out.width = "300px", fig.align="center", echo = FALSE--------------
knitr::include_graphics("gridworld.JPG")

## ------------------------------------------------------------------------
# Gridworld Environment (Sutton & Barto (2017) Example 4.1)
env = makeEnvironment("Gridworld", shape = c(4, 4), goal.states = c(0, 15))
env$states
env$actions

