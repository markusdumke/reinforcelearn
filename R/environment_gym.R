#' Gym Environment
#'
#' Reinforcement learning environment from OpenAI Gym.
#'
#' For available gym environments take a look at https://gym.openai.com/envs.
#'
#' @section Usage:
#' `makeEnvironment("gym", gym.name, ...)`
#'
#' @section Installation:
#' For installation of the python package `gym` see
#' https://github.com/openai/gym#installation.
#' Then install the R package `reticulate`.
#'
#' @param gym.name \[`character(1)`] \cr
#'   Name of gym environment, e.g. \code{"CartPole-v0"}.
#' @param ... \[`any`] \cr Arguments passed on to [makeEnvironment].
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
#' env = makeEnvironment("gym", gym.name = "MountainCar-v0")
#' env$reset()
#' env$close()
#' }
NULL

GymEnvironment = R6::R6Class("GymEnvironment",
  inherit = Environment,

  public = list(
    gym.env = NULL,
    gym.name = NULL,

    action.space = NULL,
    actions = NULL,
    action.shape = NULL,
    n.actions = NULL,
    action.space.bounds = NULL,

    state.space = NULL,
    state.shape = NULL,
    states = NULL,
    n.states = NULL,
    state.space.bounds = NULL,

    close = function() {
      self$gym.env$close()
    },

    initialize = function(gym.name, ...) {
      if (!requireNamespace("reticulate", quietly = TRUE)) {
        stop("Please install the reticulate package to use environments from OpenAI Gym.
        Also make sure you have the python package gym installed.",
          call. = FALSE)
      }
      checkmate::assertCharacter(gym.name, len = 1)
      self$gym.name = gym.name

      gym = reticulate::import("gym")
      self$gym.env = gym$make(gym.name)

      action.space.info = self$gym.env$action_space
      self$action.space = extractSpaceClass(action.space.info)

      state.space.info = self$gym.env$observation_space
      self$state.space = extractSpaceClass(state.space.info)

      if (self$action.space == "Discrete") {
        res = extractDiscreteInfo(action.space.info)
        self$n.actions = res$n
        self$actions = res$x
      }

      if (self$action.space == "Box") {
        res = extractBoxInfo(action.space.info)
        self$action.space.bounds = res$bounds
        self$action.shape = res$shape
      }

      if (self$state.space == "Discrete") {
        res = extractDiscreteInfo(state.space.info)
        self$n.actions = res$n
        self$actions = res$x
      }

      if (self$state.space == "Box") {
        res = extractBoxInfo(state.space.info)
        self$state.space.bounds = res$bounds
        self$state.shape = res$shape
      }

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

      super$initialize(step_, reset_, visualize_, ...)
    }
  )
)


extractDiscreteInfo = function(info) {
  n = info$n
  x = seq(0, n - 1)
  list(n = n, x = x)
}

extractBoxInfo = function(info) {
  list(bounds = list(info$low, info$high), shape = info$shape[[1]]) # does [[1]] work in all cases?
}

extractSpaceClass = function(info) {
  sub(".*\\.", "", class(info)[1])
}
