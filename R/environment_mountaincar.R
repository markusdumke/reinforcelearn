MountainCarBase = R6::R6Class("MountainCarBase",
  inherit = Environment,
  public = list(
    state.space = "Box",
    state.space.bounds = list(c(-1.2, 0.5), c(-0.07, 0.07)),
    velocity = NULL,
    position = NULL,

    initialize = function(...) {
      reset_ = function(self) {
        self$position = runif(1, -0.6, -0.4)
        self$velocity = 0
        matrix(c(self$position, self$velocity), ncol = 2)
      }

      step_ = function(self, action) {
        self$velocity = private$getVelocity(self, action)
        self$velocity = min(max(self$velocity, self$state.space.bounds[[2]][1]),
          self$state.space.bounds[[2]][2])
        self$position = self$position + self$velocity
        if (self$position < self$state.space.bounds[[1]][1]) {
          self$position = self$state.space.bounds[[1]][1]
          self$velocity = 0
        }
        state = matrix(c(self$position, self$velocity), ncol = 2)
        reward = -1
        if (self$position >= 0.5) {
          done = TRUE
          reward = 0
        } else {
          done = FALSE
        }
        list(state, reward, done)
      }

      super$initialize(step_, reset_, ...)
    }
  )
)

#' Mountain Car
#'
#' The classical mountain car problem for reinforcement learning.
#'
#' The classical Mountain Car task the action is one of \{0, 1, 2\},
#' in the continuous version the action is in \[-1, 1].
#'
#' @param ... \[`any`] \cr Arguments passed on to [makeEnvironment].
#'
#' @section Usage:
#' `makeEnvironment("MountainCar", ...)` \cr
#' `makeEnvironment("MountainCarContinuous", ...)`
#'
#' @md
#'
#' @inheritSection Environment Methods
#' @name MountainCar
#' @aliases MountainCarContinuous, mountain.car
#' @examples
#' env = makeEnvironment("mountain.car")
#' env$reset()
#' env$step(1L)
#'
#' env = makeEnvironment("mountain.car.continuous")
#' env$reset()
#' env$step(0.62)
NULL

#' @rdname MountainCar
#' @usage NULL
MountainCar = R6::R6Class("MountainCar",
  inherit = MountainCarBase,
  public = list(
    action.space = "Discrete",
    actions = 0:2,
    n.actions = 3L
  ),
  private = list(
    getVelocity = function(self, action) {
      self$velocity + 0.001 * (action - 1L) - 0.0025 * cos(3 * self$position)
    }
  )
)

#' @rdname MountainCar
#' @usage NULL
MountainCarContinuous = R6::R6Class("MountainCarContinuous",
  inherit = MountainCarBase,
  public = list(
    action.space = "Box",
    action.space.bounds = list(c(-1, 1))
  ),
  private = list(
    getVelocity = function(self, action) {
      force = min(max(action, self$action.space.bounds[[1]][1]), self$action.space.bounds[[1]][2])
      self$velocity + 0.0015 * force - 0.0025 * cos(3 * self$position)
    }
  )
)
