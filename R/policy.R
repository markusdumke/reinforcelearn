#' Create policy.
#'
#' Reinforcement learning policies.
#'
#' @param class \[`character(1)`] \cr
#'   Class of policy. One of `c("random", "epsilon.greedy", "greedy", "softmax")`.
#' @param ... \[`any`] \cr Optional named arguments passed on to the subclass. Alternatively
#' these can be given using the `args` argument.
#' @param args \[`list`] \cr Optional list of named arguments passed on to the
#' subclass. The arguments in ... take precedence over values in this list.
#' We strongly encourage you to use one or the other to pass arguments
#' to the function but not both.
#'
#' @return \[`list(name, args)`] List with the name and optional args.
#'   This list can then be passed onto [makeAgent], which will construct the
#'   policy accordingly.
#'
#' @md
#' @aliases Policy
#'
#' @section Policies:
#' * [RandomPolicy]
#' * [GreedyPolicy]
#' * [EpsilonGreedyPolicy]
#' * [SoftmaxPolicy]
#'
#' @export
#' @examples
#' policy = makePolicy("random")
#' policy = makePolicy("epsilon.greedy", epsilon = 0.1)
makePolicy = function(class = "random", args = list(), ...) {
  checkmate::assertChoice(class,
    c("random", "epsilon.greedy", "greedy", "softmax")) #, "gaussian"))
  checkmate::assertList(args, names = "unique")
  args = append(list(...), args)
  # remove duplicate entries in args list
  args = args[unique(names(args))]

  # fixme: check arguments of policy here
  x = list(name = class, args = args)
  class(x) = "Policy"
  x
}


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
#' @aliases GreedyPolicy
#' @export
#' @section Usage:
#' \code{makePolicy("epsilon.greedy", epsilon = 0.1)} \cr
#' \code{makePolicy("greedy")}
#'
#' @param epsilon [\code{numeric(1) in [0, 1]}] \cr
#'   Ratio of random exploration in epsilon-greedy action selection.
#'
#' @name EpsilonGreedyPolicy
#' @examples
#' policy = makePolicy("epsilon.greedy", epsilon = 0.1)
NULL

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
    initialize = function(epsilon = 0.1) {
      checkmate::assertNumber(epsilon, lower = 0, upper = 1)
      self$epsilon = epsilon
    }
  )
)

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
#' \code{makePolicy("random")}
#'
#' @name RandomPolicy
#' @examples
#' pol = makePolicy("random")
NULL

RandomPolicy = R6::R6Class("RandomPolicy",
  inherit = Policy,
  public = list(
    getActionProbs = function(Q, n.actions) {
      policy = matrix(1 / n.actions, nrow = 1, ncol = n.actions)
      policy
    }
  )
)

# GaussianPolicy = R6::R6Class("GaussianPolicy",
#   inherit = Policy,
#   public = list(
#     sampleAction = function(mean, sd) {
#       rnorm(1L, mean, sd)
#     }
#   )
# )

#' Softmax Policy
#'
#' @export
#' @section Usage:
#' \code{makePolicy("softmax")}
#'
#' @name SoftmaxPolicy
#' @examples
#' pol = makePolicy("softmax")
NULL

SoftmaxPolicy = R6::R6Class("SoftmaxPolicy",
  inherit = Policy,
  public = list(
    getActionProbs = function(Q, n.actions) {
      policy = exp(Q) / rowSums(exp(Q))
      policy
    }
  )
)
