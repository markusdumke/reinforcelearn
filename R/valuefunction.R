#' Value Function Representation
#'
#' @md
#'
#' @section Representations:
#' * [ActionValueTable]
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


#' Action Value Table
#'
#' Table representing the action value function Q.
#'
#' @export
#' @name ActionValueTable
#'
#' @examples
#' val = makeValueFunction("table", n.states = 20L, n.actions = 4L)
NULL

ActionValueTable = R6::R6Class("ActionValueTable",
  public = list(
    Q = NULL,
    step.size = NULL,

    # fixme: get number of states and actions automatically from environment
    # fixme: custom initializer, e.g. not to 0
    initialize = function(n.states, n.actions, step.size = 1) { # initializer argument
      self$Q = matrix(0, nrow = n.states, ncol = n.actions)
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

    processBatch = function(batch) {
      data = data.frame(state = unlist(batch[["state"]]), action = unlist(batch[["action"]]),
        reward = unlist(batch[["reward"]]), next.state = unlist(batch[["next.state"]]))
      data
    },

    getWeights = function() {
      self$Q
    }
  )
)

#' @export
getStateValues = function(action.vals) {
  apply(action.vals, 1L, max)
}
