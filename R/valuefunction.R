#' Value Function Representation
#'
#' @md
#'
#' @section Representations:
#' * [ActionValueNetwork]
#' * [ActionValueTable]
#'
#' @name ValueFunction
NULL

#' Action Value Network
#'
#' Neural network representing the action value function Q.
#'
#' @export
#' @name ActionValueNetwork
#'
#' @examples
#' \dontrun{
#' library(keras)
#' model = keras_model_sequential()
#' model %>% layer_dense(20, input_shape = 10, activation = "relu")
#' model %>% layer_dense(4, activation = "softmax")
#' keras::compile(model, loss = "mae", optimizer = keras::optimizer_sgd(lr = 0.4))
#'
#' val = ActionValueNetwork$new(model)
#' val$predictQ(matrix(1, ncol = 10))
#' val$train(matrix(1, ncol = 10), target = matrix(c(-1, 2, 4, -3), ncol = 4))
#' }
NULL

#' @export
ActionValueNetwork = R6::R6Class("ActionValueNetwork",
  public = list(
    model = NULL,
    preprocess = NULL,

    # keras model # fixme: add support for mxnet
    initialize = function(model, preprocess = identity) {
      self$model = model
      self$preprocess = preprocess
    },

    predictQ = function(state) {
      # state = self$preprocess(state)
      predict(self$model, state) # another function?
    },

    train = function(state, target) { # add ... argument to pass on arguments to fit
      # state = self$preprocess(state)
      keras::fit(self$model, state, target, verbose = 0L)
    },

    processBatch = function(batch) {
      data = list(
        state = do.call(rbind, batch[["state"]]), # problematic for matrix with many columns, purrr::reduce
        action = unlist(batch[["action"]]),
        reward = unlist(batch[["reward"]]),
        next.state = purrr::reduce(batch[["next.state"]], rbind)
      )
      data
    },

    getWeights = function() {
      self$model %>% get_weights()
    }
  )
)

#' Action Value Table
#'
#' Table representing the action value function Q.
#'
#' @export
#' @name ActionValueTable
#'
#' @examples
#' val = ActionValueTable$new(n.states = 3, n.actions = 4)
#' val$predictQ(matrix(1, ncol = 3))
#' val$train(2, target = matrix(c(-1, 2, 4, -3), ncol = 4))
#' val$Q
NULL

#' @export
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
