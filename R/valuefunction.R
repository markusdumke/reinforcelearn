#' Value Function Representation
#'
#' @param class \[`character(1)`] \cr Class of value function approximation.
#' @param ... \[`any`] \cr Arguments passed to subclass.
#'
#' @md
#'
#' @section Representations:
#' * [ValueTable]
#'
#' @export
makeValueFunction = function(class, ...) {
  checkmate::assertChoice(class,
    c("table")) #, "keras.neural.network", "mxnet.neural.network"))
  # fixme: check arguments here
  x = list(name = class, args = list(...)) # get properties
  class(x) = "ValueFunction"
  x
}
# comment: this could also be used for policy params -> better name?


#' Value Table
#'
#' Table representing the action value function Q.
#'
#' @export
#' @name ValueTable
#'
#' @examples
#' val = makeValueFunction("table", n.states = 20L, n.actions = 4L)
NULL

ValueTable = R6::R6Class("ValueTable",
  public = list(
    Q = NULL,
    step.size = NULL,

    # fixme: get number of states and actions automatically from environment
    # fixme: custom initializer, e.g. not to 0
    initialize = function(n.states = NULL, n.actions = 1L, step.size = 0.1,
      initial.value = NULL) { # initializer argument

      # state or action value function
      if (!is.null(initial.value)) {
        self$Q = initial.value
      } else {
        self$Q = matrix(0, nrow = n.states, ncol = n.actions)
      }
      self$step.size = step.size
    },

    predictQ = function(state) {
      self$Q[state + 1L, , drop = FALSE]
    },

    # fixme: make this vectorised -> ok
    # caveat: states must be unique!
    train = function(state, target, step.size = self$step.size) {
      self$Q[state + 1L, ] = self$Q[state + 1L, ] + step.size * (target - self$Q[state + 1L, ]) # drop = FALSE ?
    },

    # train with td error and eligibility traces
    trainWithError = function(eligibility, error, step.size = self$step.size) {
      self$Q = self$Q + step.size * error * eligibility
    },

    # processBatch = function(batch) {
    #   data = data.frame(state = unlist(batch[["state"]]), action = unlist(batch[["action"]]),
    #     reward = unlist(batch[["reward"]]), next.state = unlist(batch[["next.state"]]))
    #   data
    # },

    getWeights = function() {
      self$Q
    }
  )
)

#' Get state values.
#'
#' Get state value function from  action value function.
#'
#' @param action.vals \[`matrix`] \cr Action value matrix.
#'
#' @md
#'
#' @export
getStateValues = function(action.vals) {
  checkmate::assertMatrix(action.vals)
  apply(action.vals, 1L, max)
}
