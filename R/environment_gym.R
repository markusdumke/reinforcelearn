#' Gym Environment
#'
#' Reinforcement learning environment from OpenAI Gym.
#'
#' For OpenAI gym environments have a look at \url{https://gym.openai.com/envs}.
#'
#' @section Installation:
#' For installation of the python package \code{gym} see
#' \url{https://github.com/openai/gym#installation}.
#' Then install the R package \code{reticulate}.
#'
#' @section Usage:
#' \code{GymEnvironment$new(gym.name)}
#'
#' @param gym [\code{character(1)}] \cr
#'   Name of gym environment, e.g. \code{"CartPole-v0"}.
#'
#' @section Methods:
#' \code{$close()} Close visualization window.
#'
#' @name GymEnvironment
#' @inheritSection Environment Methods
#' @export
#'
#' @examples
#' \dontrun{
#' env = GymEnvironment$new("MountainCar-v0")
#' env$reset()
#' env$close()
#' }
NULL

#' @export
GymEnvironment = R6::R6Class("GymEnvironment",
  inherit = Environment,

  public = list(
    gym.env = NULL,
    gym.name = NULL,

    close = function() {
      self$gym.env$close()
    },

    initialize = function(gym.name) {
      if (!requireNamespace("reticulate", quietly = TRUE)) {
        stop("Please install the reticulate package to use environments from OpenAI Gym.
        Also make sure you have the python package gym installed.",
          call. = FALSE)
      }
      checkmate::assertCharacter(gym.name, len = 1)
      self$gym.name = gym.name

      gym = reticulate::import("gym")
      self$gym.env = gym$make(gym.name)

      step_ = function(env, action) {
        res = self$gym.env$step(action)
        res[1:3]
      }

      reset_ = function() {
        state = self$gym.env$reset()
        state
      }

      visualize_ = function(self) {
        self$gym.env$render()
      }

      super$initialize(step_, reset_, visualize_)
    }
  )
)
