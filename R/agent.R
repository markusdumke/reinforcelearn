#' Create Agent.
#'
#' @param policy \[`Policy`] \cr A policy created by [makePolicy].
#' @param val.fun \[`ActionValueTable`] \cr Value function representation.
#' @param algorithm \[`Algorithm`] \cr An algorithm.
#'
#' @md
#'
#' @export
makeAgent = function(policy, val.fun = NULL, algorithm = NULL) {
  checkmate::assertClass(policy, "Policy")
  Agent$new(policy, val.fun, algorithm)
}

Agent = R6::R6Class("Agent",
  public = list(
    policy = NULL,

    preprocess = NULL,
    observe = NULL,
    learn = NULL,

    learn.logical = TRUE,

    # store all encountered states, actions, rewards with corresponding episode number
    # possibly write to file?
    # set maximum size for preallocation?
    history = list(),
    # logging function

    initialize = function(policy, val.fun, algorithm) {
      self$policy = policy

      if (missing(algorithm)) {
        self$learn.logical = FALSE
      }

      self$observe = function(state, action, reward, next.state) {}
      self$learn = function(discount = 1) {}
      self$preprocess = identity

    },

    act = function(state) {
      state = self$preprocess(state)
      policy.probs = self$policy$getActionProbs(NULL, n.actions = 4L) # how to get correct number of actions automatically?
      action = self$policy$sampleAction(policy.probs)
      action
    }
  )
)
