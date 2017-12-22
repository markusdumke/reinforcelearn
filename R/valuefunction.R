#' Value Function Representation
#'
#' A representation of the value function.
#'
#' @param class \[`character(1)`] \cr Class of value function approximation.
#'   One of `c("table", "neural.network")`.
#' @inheritParams makePolicy
#'
#' @return \[`list(name, args)`] List with the name and optional args.
#'   This list can then be passed onto [makeAgent], which will construct the
#'   value function accordingly.
#'
#' @md
#'
#' @section Representations:
#' * [ValueTable]
#' * [ValueNetwork]
#'
#' @export
#' @examples
#' val = makeValueFunction("table", n.states = 16L, n.actions = 4L)
#' # If the number of states and actions is not supplied, the agent will try
#' # to figure these out from the environment object during interaction.
#' val = makeValueFunction("table")
makeValueFunction = function(class, args = list(), ...) {
  checkmate::assertChoice(class, c("table", "neural.network")) #, "keras.neural.network", "mxnet.neural.network"))
  # fixme: check arguments here
  checkmate::assertList(args, names = "unique")
  args = append(list(...), args)
  # remove duplicate entries in args list
  args = args[unique(names(args))]

  x = list(name = class, args = args)
  class(x) = "ValueFunction"
  x
}
# comment: this could also be used for policy params -> better name?


#' Value Table
#'
#' Table representing the action value function Q.
#'
#' You can specify the shape of the value table. If omitted the agent will try
#' to configure these automatically from the environment during interaction
#' (therefore the environment needs to have a `n.states` and `n.actions` attribute).
#'
#' @section Usage:
#' `makeValueFunction("table", n.states = NULL, n.actions = 1L,
#'   step.size = 0.1, initial.value = NULL)`
#'
#' @param n.states \[`integer(1)`] \cr Number of states (rows in the value function).
#' @param n.actions \[`integer(1)`] \cr Number of actions (columns in the value function).
#' @param step.size \[`numeric(1)`] \cr Step size (learning rate) for gradient descent update.
#'
#' @name ValueTable
#' @aliases table
#' @md
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
      initial.value = NULL) {

      checkmate::assertInt(n.states, lower = 1)
      checkmate::assertInt(n.actions, lower = 1)
      checkmate::assertNumber(step.size, lower = 0)
      checkmate::assertMatrix(initial.value, null.ok = TRUE)

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

#' Value Network
#'
#' Neural network representing the action value function Q.
#'
#' @section Usage:
#' `makeValueFunction("neural.network", model)`
#'
#' @param model \[`keras model`] \cr A keras model.
#'   Make sure that the model has been compiled.
#'
#' @name ValueNetwork
#' @aliases neural.network
#' @md
#'
#' @examples
#' \dontrun{
#' library(keras)
#' model = keras_model_sequential()
#' model %>% layer_dense(20, input_shape = 10, activation = "relu")
#' model %>% layer_dense(4, activation = "softmax")
#' keras::compile(model, loss = "mae", optimizer = keras::optimizer_sgd(lr = 0.4))
#'
#' val = makeValueFunction("neural.network", model = model)
#' }
NULL

ValueNetwork = R6::R6Class("ValueNetwork",
  public = list(
    model = NULL,

    # keras model # fixme: add support for mxnet
    initialize = function(model) {
      checkmate::assertClass(model, "keras.models.Sequential")
      self$model = model
    },

    predictQ = function(state) {
      predict(self$model, state) # another function?
    },

    train = function(state, target) { # add ... argument to pass on arguments to fit
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
