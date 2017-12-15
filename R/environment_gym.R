#' Gym Environment
#'
#' Reinforcement learning environment from OpenAI Gym.
#'
#' For available gym environments take a look at https://gym.openai.com/envs.
#'
#' @section Usage:
#' `makeEnvironment("Gym", gym.name)`
#'
#' @section Installation:
#' For installation of the python package `gym` see
#' https://github.com/openai/gym#installation.
#' Then install the R package `reticulate`.
#'
#' @param gym.name \[`character(1)`] \cr
#'   Name of gym environment, e.g. \code{"CartPole-v0"}.
#'
#' @md
#'
#' @section Methods:
#' * `$close()`
#'   Close visualization window.
#'
#' @name GymEnvironment
#' @inheritSection Environment Methods
#' @export
#'
#' @examples
#' \dontrun{
#' # Create an OpenAI Gym environment.
#' # Make sure you have Python, gym and reticulate installed.
#' env = makeEnvironment("Gym", "MountainCar-v0")
#' env$reset()
#' env$close()
#' }
NULL

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

      # # fixme: extract info about state, action space
      # action.space.info = self$gym.env$action_space
      # # self$action.space = action.space.info$name
      #
      # if (action.space.info$name == "Discrete") {
      #   self$n.actions = action.space.info$n
      #   self$actions = seq(0, self$n.actions - 1)
      # }
      #
      # if (action.space.info$name == "Box") {
      #   self$action.shape = action.space.info$shape[[1]]
      #   self$action.space.bounds = list()
      #   for (i in seq_len(self$action.shape)) {
      #     self$action.space.bounds = append(self$action.space.bounds,
      #       list(c(action.space.info$low[[i]], action.space.info$high[[i]])))
      #   }
      # }
      #
      # state.space.info = self$gym.env$observation_space
      # self$state.space = state.space.info$name
      #
      # if (state.space.info$name == "Discrete") {
      #   self$n.states = state.space.info$n
      #   self$states = seq(0, self$n.states - 1)
      # }
      #
      # if (state.space.info$name == "Box") {
      #   self$state.shape = state.space.info$shape[[1]]
      #   self$state.space.bounds = list()
      #   for (i in seq_len(self$state.shape)) {
      #     self$state.space.bounds = append(self$state.space.bounds,
      #       list(c(state.space.info$low[[i]], state.space.info$high[[i]])))
      #   }
      # }

      step_ = function(self, action) {
        res = self$gym.env$step(action)
        res[1:3]
      }

      reset_ = function(self) {
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
