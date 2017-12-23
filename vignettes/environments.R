## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(message = TRUE, eval = TRUE, collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(reinforcelearn)

## ---- out.width = "200px", fig.align="center", echo = FALSE--------------
knitr::include_graphics("mountaincar.JPG")

## ------------------------------------------------------------------------
reset = function(self) {
  position = runif(1, -0.6, -0.4)
  velocity = 0
  state = matrix(c(position, velocity), ncol = 2)
  state
}

## ------------------------------------------------------------------------
step = function(self, action) {
  position = self$state[1]
  velocity = self$state[2]
  velocity = (action - 1L) * 0.001 + cos(3 * position) * (-0.0025)
  velocity = min(max(velocity, -0.07), 0.07)
  position = position + velocity
  if (position < -1.2) {
    position = -1.2
    velocity = 0
  }
  state = matrix(c(position, velocity), ncol = 2)
  reward = -1
  if (position >= 0.5) {
    done = TRUE
    reward = 0
  } else {
    done = FALSE
  }
  list(state, reward, done)
}

## ------------------------------------------------------------------------
env = makeEnvironment(step = step, reset = reset)

## ---- eval = FALSE-------------------------------------------------------
#  # Create a gym environment.
#  env = makeEnvironment("gym", gym.name = "MountainCar-v0")

## ------------------------------------------------------------------------
# State transition array
P = array(0, c(2, 2, 2))
P[, , 1] = matrix(c(0.5, 0.5, 0, 1), 2, 2, byrow = TRUE)
P[, , 2] = matrix(c(0.1, 0.9, 0, 1), 2, 2, byrow = TRUE)

# Reward matrix
R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)

env = makeEnvironment("mdp", transitions = P, rewards = R)

## ---- out.width = "200px", fig.align="center", echo = FALSE--------------
knitr::include_graphics("gridworld.JPG")

## ------------------------------------------------------------------------
# Gridworld Environment (Sutton & Barto (2017) Example 4.1)
env = makeEnvironment("gridworld", shape = c(4, 4), goal.states = c(0, 15))

## ------------------------------------------------------------------------
env = makeEnvironment("gridworld", shape = c(4, 4), 
  goal.states = 0L, initial.state = 15L)

## ------------------------------------------------------------------------
# The initial state of the environment.
env$reset()

env$visualize()

# Actions are encoded as integers.
env$step(0L)

env$visualize()

# But can also have character names.
env$step("left")

env$visualize()

## ------------------------------------------------------------------------
env = makeEnvironment("mountain.car")
env$n.actions
env$state.space.bounds

## ------------------------------------------------------------------------
env = makeEnvironment("gridworld", shape = c(4, 4), 
  goal.states = 0L, initial.state = 15L, discount = 0.99)

env$step("up")
env$n.step
env$episode.return

env$step("left")
env$n.step
env$episode.return

