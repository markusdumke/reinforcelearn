# ActionValueNetwork
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
      state = self$preprocess(state)
      predict(self$model, state) # another function?
    },
    train = function(state, target) {
      state = self$preprocess(state)
      keras::fit(self$model, state, target, verbose = 0)
    }
  )
)

# ActionValueTable
ActionValueTable = R6::R6Class("ActionValueTable",
  public = list(
    Q = NULL,
    step.size = NULL,
    # fixme: get number of states and actions automatically from environment
    # fixme: custom initializer, e.g. not to 0
    initialize = function(n.states, n.actions, step.size) {
      self$Q = matrix(0, nrow = n.states, ncol = n.actions)
      self$step.size = step.size
    },
    predictQ = function(state) {
      self$Q[state + 1, ]
    },
    # fixme: make this vectorised
    train = function(state, target) {
      self$Q[state + 1, ] = self$Q[state + 1, ] + self$step.size * (target - self$Q[state + 1, ])
    }
  )
)
