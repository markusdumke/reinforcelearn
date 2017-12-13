#' Policy
#'
#' Reinforcement learning policies.
#'
#' @md
#'
#' @section Policies:
#' * [RandomPolicy]
#' * [GreedyPolicy]
#' * [EpsilonGreedyPolicy]
#' * [GaussianPolicy]
#' * [SoftmaxPolicy]
#'
#' @section Methods:
#' \code{$sampleAction(policy)} Sample action from policy probabilities.
#'
#' @name Policy
NULL

Policy = R6::R6Class("Policy",
  public = list(
    sampleAction = function(policy) {
      action = sample(seq_along(policy), prob = policy,
        size = 1, replace = TRUE) - 1L
      action
    }
  )
)

#' Epsilon Greedy Policy
#'
#' @export
#' @section Usage:
#' \code{EpsilonGreedyPolicy$new(epsilon)} \cr
#' \code{GreedyPolicy$new()}
#'
#' @param epsilon [\code{numeric(1) in [0, 1]}] \cr
#'   Ratio of random exploration in epsilon-greedy action selection.
#'
#' @inheritSection Policy Methods
#' @name EpsilonGreedyPolicy
#' @examples
#' pol = EpsilonGreedyPolicy$new(epsilon = 0.1)
#' (probs = pol$getActionProbs(matrix(c(1:3), ncol = 3), n.actions = 3))
#' pol$sampleAction(probs)
NULL

#' @export
EpsilonGreedyPolicy = R6::R6Class("EpsilonGreedyPolicy",
  inherit = Policy,
  public = list(
    epsilon = NULL,
    getActionProbs = function(Q, n.actions) { # fixme: break ties
      greedy.action = nnet::which.is.max(Q)
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

#' @export
#' @rdname EpsilonGreedyPolicy
#' @usage NULL
GreedyPolicy = R6::R6Class("GreedyPolicy",
  # inherit = EpsilonGreedyPolicy,
  public = list(
    getActionProbs = function(Q, n.actions) {
      greedy.action = nnet::which.is.max(Q) # this is duplicate code!
      policy = matrix(0, nrow = 1, ncol = n.actions)
      policy[, greedy.action] = 1
      policy
    }
  )
)

#' Random Policy
#'
#' @export
#' @section Usage:
#' \code{RandomPolicy$new()}
#'
#' @inheritSection Policy Methods
#' @name RandomPolicy
#' @examples
#' pol = RandomPolicy$new()
#' (probs = pol$getActionProbs(n.actions = 4))
#' (probs = pol$getActionProbs(n.actions = 5))
#' pol$sampleAction(probs)
NULL

#' @export
RandomPolicy = R6::R6Class("RandomPolicy",
  inherit = Policy,
  public = list(
    getActionProbs = function(Q, n.actions) {
      policy = matrix(1 / n.actions, nrow = 1, ncol = n.actions)
      policy
    }
  )
)

#' Gaussian Policy
#'
#' @export
#' @section Usage:
#' \code{GaussianPolicy$new()}
#'
#' @inheritSection Policy Methods
#' @name GaussianPolicy
#' @examples
#' pol = GaussianPolicy$new()
#' pol$sampleAction(mean = 10, sd = 1)
NULL

#' @export
GaussianPolicy = R6::R6Class("GaussianPolicy",
  inherit = Policy,
  public = list(
    sampleAction = function(mean, sd) {
      rnorm(1L, mean, sd)
    }
  )
)

#' Softmax Policy
#'
#' @export
#' @section Usage:
#' \code{SoftmaxPolicy$new()}
#'
#' @inheritSection Policy Methods
#' @name SoftmaxPolicy
#' @examples
#' pol = SoftmaxPolicy$new()
#' (probs = pol$getActionProbs(matrix(c(1:3), ncol = 3)))
#' pol$sampleAction(probs)
NULL

#' @export
SoftmaxPolicy = R6::R6Class("SoftmaxPolicy",
  inherit = Policy,
  public = list(
    getActionProbs = function(Q, n.actions) {
      policy = exp(Q) / rowSums(exp(Q))
      policy
    }
  )
)

# ideas: DiscretePolicy class
