#' Reinforcement Learning Environment
#'
#' @section Usage:
#' \code{Environment$new(step, reset, visualize)}
#'
#' @param step [\code{function(self, action)}] \cr
#'   Custom step function.
#' @param reset [\code{function()}] \cr
#'   Custom reset function.
#' @param visualize [\code{function(self)}] \cr
#'   Optional custom visualize function.
#'
#' @section Methods:
#' \code{$new()} Initializes a new environment.
#'
#' \code{$reset()} Starts a new episode.
#' Returns the \code{state}.
#'
#' \code{$step(action)} Take action in environment.
#' Returns a list with \code{state}, \code{reward}, \code{done}.
#'
#' \code{$visualize()} Visualizes environment.
#'
#' @name Environment
#' @export
#'
#' @examples
#' step = function(self, action) {
#'   state = list(mean = action + rnorm(1), sd = runif(1))
#'   reward = rnorm(1, state[[1]], state[[2]])
#'   done = FALSE
#'   list(state, reward, done)
#' }
#'
#' reset = function() {
#'   state = list(mean = 0, sd = 1)
#'   state
#' }
#'
#' env = Environment$new(step, reset)
#' env$reset()
#' env$step(100)
NULL

#' @export
Environment = R6::R6Class("Environment",
  public = list(
    n.step = 0L,
    episode = 0L,
    episode.step = 0L,
    episode.return = 0,
    state = NULL,
    reward = NULL,
    done = FALSE,
    discount = NULL,

    reset = function() {
      self$episode.step = 0L
      self$episode.return = 0
      self$done = FALSE
      self$state = private$reset_()
      self$state
    },

    step = function(action) {
      self$n.step = self$n.step + 1L
      self$episode.step = self$episode.step + 1L
      res = private$step_(self, action)
      self$episode.return = self$episode.return +
        self$discount ^ self$episode.step * res[[2]]
      self$state = res[[1]]
      self$reward = res[[2]]
      self$done = res[[3]]
      if (self$done) {
        self$episode = self$episode + 1L
      }
      list(state = res[[1]], reward = res[[2]], done = res[[3]])
    },

    # an optional visualization function
    visualize = function() {
      private$visualize_(self)
    },

    initialize = function(step, reset, visualize, discount = 1) {
      private$step_ = step
      private$reset_ = reset
      self$discount = discount
      if (!missing(visualize)) {
        private$visualize_ = visualize
      }
    }
  ),

  private = list(
    # step_: custom step method depending on problem that returns list with
    #   next state, reward, done
    step_ = NULL,
    # reset_: custom reset method depending on problem that returns state
    reset_ = NULL,
    visualize_ = NULL
  )
)
