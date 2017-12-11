#' ---
#' title: "Environments"
#' author: Markus Dumke
#' output: github_document
#' ---

#+ setup, include=FALSE
library(knitr)
opts_chunk$set(comment = "#>", collapse = FALSE, message = FALSE)

#'
# Switch to branch "markus"
devtools::load_all()

#' ----
#'

#' ### Environment components
#'
#' - R6 class
#'       - step(action)
#'         - returns list: state, reward, done, (info)
#'       - reset()
#'         - returns state
#'       - visualize()
#'         - user-defined visualization of environment
#'       - ...
#'         - subclass specific
#'

#' ----
#'

#' ### Environment classes
#'
#' - Environment
#'       - super R6 class
#'       - can be called with `Environment$new(step, reset, visualize)`
#'       - for cusom environments
#'
#' - GymEnvironment
#'       - subclass of `Environment`
#'       - uses `reticulate::import("gym")`
#'       - can be called with `GymEnvironment$new(gym.name)`
#'
#' - MdpEnvironment
#'       - subclass of `Environment`
#'       - can be called with `MdpEnvironment$new(transitions, rewards, ...)`
#'
#' - GridworldEnvironment
#'       - subclass of `MdpEnvironment`
#'       - can be called with `GridworldEnvironment$new(shape, goal.states, ...)`
#'

#' ----
#'

#' ### Environment examples

#' ### MDP
P = array(0, c(2, 2, 2))
P[, , 1] = matrix(c(0.5, 0.5, 0, 1), 2, 2, byrow = TRUE)
P[, , 2] = matrix(c(0, 1, 0, 1), 2, 2, byrow = TRUE)
R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)
mdp = MdpEnvironment$new(transitions = P, rewards = R)
mdp$reset()
mdp$step(1)

#' ----
#'

#' ### Gridworld
grid = GridworldEnvironment$new(shape = c(4, 4), goal.states = c(0), initial.state = 15)
grid$reset()
grid$visualize()
grid$step(0L)
grid$visualize()

#' ----
#'

#' ### Gym
gym = GymEnvironment$new(gym.name = "MountainCar-v0")
gym$reset()
gym$step(1L)
for (i in 1:200) {
  gym$step(sample(0:2, 1))
  gym$visualize()
}
gym$gym.env$close()

#' ----
#'

#' ### Custom user-defined environment
#' here: reimplementation of Mountain Car example
# returns state
reset = function() {
  position = runif(1, -0.6, -0.4)
  velocity = 0
  state = matrix(c(position, velocity), ncol = 2)
  state
}

# returns list: state, reward, done
step = function(env, action) {
  position = env$state[1]
  velocity = env$state[2]
  velocity = velocity + 0.001 * (action - 1) - 0.0025 * cos(3 * position)
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

mcar = Environment$new(step, reset)
mcar$reset()
mcar$step(1L)
