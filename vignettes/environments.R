## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(reinforcelearn)

transitions = gridworld$transitions
rewards = gridworld$rewards
env = makeEnvironment(transitions = transitions, rewards = rewards)

## ------------------------------------------------------------------------
env$action.space
env$n.states

## ---- eval = FALSE-------------------------------------------------------
#  CartPole = makeEnvironment("CartPole-v0")

## ---- eval = FALSE-------------------------------------------------------
#  CartPole$reset()
#  for (i in 1:200) {
#    action = sample(CartPole$actions, 1)
#    CartPole$step(action)
#  }
#  CartPole$close()

## ------------------------------------------------------------------------
MountainCar = R6::R6Class("MountainCar", 
  public = list(
    action.space = "Discrete",
    actions = c(0, 1, 2),
    n.actions = 3,
    state.space = "Box",
    state.space.bounds = list(c(-1.2, 0.6), c(-0.07, 0.07)),
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
      if (self$velocity < self$state.space.bounds[[2]][1]) {
        self$velocity = self$state.space.bounds[[2]][1]
      }
      if (self$velocity > self$state.space.bounds[[2]][2]) {
        self$velocity = self$state.space.bounds[[2]][2]
      }
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
    },
    
    close = function() {
      invisible(self)
    }
  )
)

## ------------------------------------------------------------------------
m = MountainCar$new()
set.seed(123456)
m$reset()
while(!m$done) {
  action = sample(m$actions, 1)
  m$step(action)
}
print(paste("Episode finished after", m$n.steps, "steps."))

