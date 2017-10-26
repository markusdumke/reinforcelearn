## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(message = FALSE, eval = TRUE, collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(reinforcelearn)
set.seed(1)

## ------------------------------------------------------------------------
# State transition array
P = array(0, c(2, 2, 2))
P[, , 1] = matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow = TRUE)
P[, , 2] = matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow = TRUE)
print(P)
# Reward matrix
R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)
print(R)
env = makeEnvironment(transitions = P, rewards = R)

## ------------------------------------------------------------------------
P = array(0, c(2, 2, 2))
P[, , 1] = matrix(c(0.5, 0.5, 0, 1), 2, 2, byrow = TRUE)
P[, , 2] = matrix(c(0.1, 0.9, 0, 1), 2, 2, byrow = TRUE)
print(P)

env = makeEnvironment(transitions = P, rewards = R)
print(env$terminal.states)

## ------------------------------------------------------------------------
env = makeEnvironment(transitions = P, rewards = R, initial.state = 0)
env$reset()
print(env)

## ------------------------------------------------------------------------
# Specify a custom probability distribution for the starting state.
reset = function() {
  p = c(0.2, 0.8)
  sample(0:1, prob = p, size = 1)
}
env = makeEnvironment(transitions = P, rewards = R, reset = reset)
env$reset()
print(env)

## ------------------------------------------------------------------------
R = array(0, c(2, 2, 2))
R[, 1, ] = 1
R[2, 2, 2] = 10
print(R)

env = makeEnvironment(transitions = P, rewards = R)

env$reset()
env$step(1)
print(env)

## ------------------------------------------------------------------------
sampleReward = function(state, action, n.state) {
  if (n.state == 0 & action == 1L) {
    0
  } else {
    rnorm(1)
  }
}
env = makeEnvironment(transitions = P, sampleReward = sampleReward)
env$reset()
env$step(0)
print(env)

## ---- out.width = "300px", fig.align="center", echo = FALSE--------------
knitr::include_graphics("gridworld.JPG")

## ------------------------------------------------------------------------
# Gridworld Environment (Sutton & Barto (2017) Example 4.1)
env = makeGridworld(shape = c(4, 4), goal.states = c(0, 15))
print(env$states)
print(env$actions)

# Identical to the above call
env = gridworld()

# Same gridworld, but with diagonal moves
env = makeGridworld(shape = c(4, 4), goal.states = c(0, 15), 
  diagonal.moves = TRUE)
print(env$actions)

## ------------------------------------------------------------------------
# Gridworld with 10% random transitions
env = makeGridworld(shape = c(4, 4), goal.states = c(0, 15), stochasticity = 0.1)

## ---- out.width = "500px", fig.align = "center", echo = FALSE------------
knitr::include_graphics("cliff.JPG")

## ------------------------------------------------------------------------
# Cliff Walking (Sutton & Barto (2017) Example 6.6)   
env = makeGridworld(shape = c(4, 12), goal.states = 47, 
  cliff.states = 37:46, reward.step = - 1, reward.cliff = - 100, 
  cliff.transition.states = 36, initial.state = 36)

# Identical to the above call
env = cliff()

## ---- out.width = "350px", fig.align = "center", echo = FALSE------------
knitr::include_graphics("windygrid.PNG")

## ------------------------------------------------------------------------
# Windy Gridworld (Sutton & Barto (2017) Example 6.5) 
env = makeGridworld(shape = c(7, 10), goal.states = 37, 
  reward.step = - 1, wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0), initial.state = 30)

# Identical to the above call
env = windyGridworld()

## ---- eval = FALSE-------------------------------------------------------
#  # Create an OpenAI Gym environment.
#  # Make sure you have Python and Gym installed.
#  # Start server from within R.
#  package.path = system.file(package = "reinforcelearn")
#  path2pythonfile = paste0(package.path, "/gym_http_server.py")
#  system2("python", args = path2pythonfile, stdout = NULL, wait = FALSE, invisible = FALSE)
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
mountainCar = R6::R6Class("MountainCar", 
  public = list(
    action.space = "Discrete",
    actions = 0:2,
    n.actions = 3,
    state.space = "Box",
    state.space.bounds = list(c(-1.2, 0.5), c(-0.07, 0.07)),
    done = FALSE,
    n.steps = 0,
    state = NULL,
    previous.state = NULL,
    reward = NULL,
    velocity = NULL,
    position = NULL,
    
    reset = function() {
      self$n.steps = 0
      self$previous.state = NULL
      self$done = FALSE
      self$position = runif(1, - 0.6, - 0.4)
      self$velocity = 0
      self$state = matrix(c(self$position, self$velocity), ncol = 2)
      invisible(self)
    },
    
    step = function(action) {
      self$previous.state = self$state
      self$n.steps = self$n.steps + 1
      
      self$velocity = self$velocity + 0.001 * (action - 1) -
        0.0025 * cos(3 * self$position)
      self$velocity = min(max(self$velocity, self$state.space.bounds[[2]][1]), 
        self$state.space.bounds[[2]][2])
      self$position = self$position + self$velocity
      if (self$position < self$state.space.bounds[[1]][1]) {
        self$position = self$state.space.bounds[[1]][1]
        self$velocity = 0
      }
      
      self$state = matrix(c(self$position, self$velocity), ncol = 2)
      self$reward = - 1
      if (self$position >= 0.5) {
        self$done = TRUE
        self$reward = 0
      }
      invisible(self)
    }
  )
)

## ------------------------------------------------------------------------
m = mountainCar$new()
set.seed(123456)
m$reset()
while (!m$done) {
  action = sample(m$actions, 1)
  m$step(action)
}
print(paste("Episode finished after", m$n.steps, "steps."))

## ------------------------------------------------------------------------
# The classical mountain car problem.
m = mountainCar()
m$reset()
m$step(1)
print(m)

## ------------------------------------------------------------------------
# Mountain car with a continuous action space
m = mountainCar(action.space = "Continuous")
m$reset()
print(m)
m$step(0.27541)
print(m)

