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
#' \code{EpsilonGreedyPolicy$new()}
#' @name EpsilonGreedyPolicy
NULL

#' @export
EpsilonGreedyPolicy = R6::R6Class("EpsilonGreedyPolicy",
  inherit = Policy,
  public = list(
    epsilon = NULL,
    getActionProbs = function(Q, n.actions) { # fixme: break ties
      greedy.action = which.max(Q)
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

#' Random Policy
#'
#' @export
#' @section Usage:
#' \code{RandomPolicy$new()}
#' @name RandomPolicy
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
#' @name GaussianPolicy
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
#' @name SoftmaxPolicy
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
