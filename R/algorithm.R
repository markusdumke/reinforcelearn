#' Make reinforcement learning algorithm.
#'
#' @param class \[`character(1)`] \cr Algorithm. One of `c("qlearning")`.
#' @inheritParams makePolicy
#'
#' @md
#'
#' @section Representations:
#' * [QLearning]
#'
#' @export
#' @examples
#' alg = makeAlgorithm("qlearning")
makeAlgorithm = function(class, args = list(), ...) {
  checkmate::assertChoice(class,
    c("qlearning"))#, "sarsa"))
  checkmate::assertList(args, names = "unique")
  args = append(list(...), args)
  # remove duplicate entries in args list
  args = args[unique(names(args))]

  x = list(name = class, args = args)
  class(x) = "Algorithm"
  x
}


#' Q-Learning
#'
#' Q-Learning algorithm.
#'
#' To use eligibility traces specify `lambda` and `traces`.
#'
#' @section Usage:
#' `makeAlgorithm("qlearning", lambda, traces)`
#'
#' @param lambda \[`numeric(1)` in (0, 1)] \cr Trace decay parameter.
#' @param traces \[`character(1)`] \cr Type of eligibility trace update. One of `c("replace", "accumulate")`.
#'
#' @name QLearning
#' @aliases qlearning
#'
#' @seealso [Eligibility]
#'
#' @md
#'
#' @examples
#' alg = makeAlgorithm("qlearning", lambda = 0.8, traces = "accumulate")
NULL

QLearning = R6::R6Class("QLearning",
  public = list(
    getTarget = function(reward, action.values, discount) {
      reward + discount * apply(action.values, 1L, max)
    }
  )
)

# Sarsa = R6::R6Class("Sarsa",
#   public = list(
#     getTarget = function(reward, action.values, discount, next.action) {
#       reward + discount * action.values[, next.action + 1L]
#     }
#   )
# )
