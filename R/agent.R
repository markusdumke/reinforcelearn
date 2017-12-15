#' Create Agent.
#'
#' @param policy \[`Policy`] \cr A policy created by [makePolicy].
#' @param val.fun \[`ActionValueTable`] \cr Value function representation.
#' @param algorithm \[`Algorithm`] \cr An algorithm.
#'
#' @md
#'
#' @export
makeAgent = function(policy, val.fun = NULL, algorithm = NULL) { # better defaults?
  checkmate::assertClass(policy, "Policy")
  checkmate::assertClass(val.fun, "ValueFunction", null.ok = TRUE)
  Agent$new(policy, val.fun, algorithm)
}

Agent = R6::R6Class("Agent",
  public = list(
    action = NULL,
    previous.action = NULL,

    policy = NULL,
    val.fun = NULL,
    algorithm = NULL,

    preprocess = NULL,
    observe = NULL,
    learn = NULL,

    learn.logical = TRUE,

    train.data = NULL,

    act2 = NULL,

    # store all encountered states, actions, rewards with corresponding episode number
    # possibly write to file?
    # set maximum size for preallocation?
    history = list(),
    # logging function

    initialize = function(policy, val.fun, algorithm) {

      self$policy = switch(policy$name,
        random = RandomPolicy$new(),
        epsilon.greedy = do.call(EpsilonGreedyPolicy$new, policy$args),
        greedy = GreedyPolicy$new(...),
        softmax = SoftmaxPolicy$new(...)
      )

      if (policy$name == "random") {
        self$act2 = function(state) {
          policy.probs = self$policy$getActionProbs(NULL, n.actions = 4L)
          action = self$policy$sampleAction(policy.probs)
        }
      } else {
        self$act2 = function(state) {
          action.vals = self$val.fun$predictQ(state)
          policy.probs = self$policy$getActionProbs(action.vals, n.actions = length(action.vals))
          action = self$policy$sampleAction(policy.probs)
        }
      }

      if (!is.null(val.fun)) { # name this model?
        self$val.fun = switch(val.fun$name,
          table = do.call(ActionValueTable$new, val.fun$args)
        )
      }

      if (!is.null(algorithm)) {
        self$algorithm = switch(algorithm$name,
          qlearning = QLearning$new()
        )
      }

      if (missing(val.fun) || missing(algorithm)) {
        self$learn.logical = FALSE
      }

      if (algorithm$name == "qlearning") {
        self$observe = function(state, action, reward, next.state) {
          state = self$preprocess(state)
          next.state = self$preprocess(next.state)
          self$train.data = list(state = state, action = action, reward = reward, next.state = next.state)
        }
        self$learn = function(env) {
          q.old = self$val.fun$predictQ(self$train.data$state)
          q.new = self$val.fun$predictQ(self$train.data$next.state)
          target = self$algorithm$getTarget(self$train.data$reward, q.new, discount = env$discount)
          self$val.fun$train(self$train.data$state, target)
        }

      } else {
        self$observe = function(state, action, reward, next.state) {} # agent observe n.actions from environment here?
        self$learn = function(env) {}
      }

      self$preprocess = identity

    },

    act = function(state) {
      state = self$preprocess(state)
      action = self$act2(state)
      self$previous.action = action
      self$action = action
      action
    }
  )
)
