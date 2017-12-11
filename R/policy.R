# a class to represent the policy
Policy = R6::R6Class("Policy",
  public = list(
    sampleAction = function(policy) {
      action = sample(seq_along(policy), prob = policy,
        size = 1, replace = TRUE) - 1L
      action
    }
  )
)

EpsilonGreedyPolicy = R6::R6Class("EpsilonGreedyPolicy",
  inherit = Policy,
  public = list(
    epsilon = NULL,
    getActionProbs = function(Q) { # fixme: break ties
      greedy.action = which.max(Q)
      n.actions = length(Q)
      policy = matrix(0, nrow = 1, ncol = n.actions)
      policy[, greedy.action] = 1 - self$epsilon
      policy = policy + self$epsilon / n.actions
      policy
    },
    initialize = function(epsilon) {
      self$epsilon = epsilon
    }
  )
)

RandomPolicy = R6::R6Class("RandomPolicy",
  inherit = Policy,
  public = list(
    getActionProbs = function(Q) {
      n.actions = length(Q)
      policy = matrix(1 / n.actions, nrow = 1, ncol = n.actions)
      policy
    }
  )
)

# makePolicy = function(type, epsilon) {
#   if (type == "epsilon-greedy") {
#     policy = EpsilonGreedyPolicy$new(epsilon)
#   }
#   policy
# }

# # get epsilon-greedy policy with respect to Q
# # Q is a one-column matrix / one-row matrix
# epsilonGreedyPolicy = function(Q, epsilon) {
#   greedy.action = which.max(Q)
#   n.actions = length(Q)
#   policy = matrix(0, nrow = 1, ncol = n.actions)
#   policy[, greedy.action] = 1 - epsilon
#   policy = policy + epsilon / n.actions
#   policy
# }

# # sample action from policy
# sampleActionFromPolicy = function(policy) {
#   action = sample(seq_along(policy), prob = policy, size = 1, replace = TRUE) - 1L
#   action
# }
