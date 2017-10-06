#' Mountain Car
#' 
#' @param action.space [\code{character(1)}] \cr
#'   Can be \code{"Discrete"} or \code{"Continuous"}. 
#'   The classical Mountain Car task has three discrete actions, 
#'   the continuous version one action in a box from -1 to 1.
#' @references Sutton and Barto (2017): Reinforcement Learning: An Introduction
#' @export
#' @examples 
#' # Classic Mountain Car task
#' m = MountainCar()
#' m$reset()
#' m$step(1)
#' print(m)
#' 
#' # Mountain Car with continuous action space
#' m = MountainCar(action.space == "Continuous")
#' m$reset()
#' m$step(0.27541)
#' print(m)
#' 
MountainCar = function(action.space = "Discrete") {
  checkmate::assertChoice(action.space, c("Discrete", "Continuous"))
  if (action.space == "Discrete") {
    MountainCarDiscrete$new()
  } else {
    MountainCarContinuous$new()
  }
}

MountainCarDiscrete = R6::R6Class("MountainCar",
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
    },

    close = function() {
      invisible(self)
    }
  )
)

MountainCarContinuous = R6::R6Class("MountainCarContinuous",
  public = list(
    action.space = "Box",
    action.space.bounds = list(c(-1, 1)),
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
      
      force = min(max(action, - 1), 1)
      
      self$velocity = self$velocity + 0.0015 * force - 0.0025 * cos(3 * self$position)
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
    },
    
    close = function() {
      invisible(self)
    }
  )
)
