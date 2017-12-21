#' Make reinforcement learning algorithm.
#'
#' @param class \[`character(1)`] \cr Algorithm. One of `c("qlearning")`.
#' @param ... \[`any`] \cr Arguments passed on to the specific algorithm.
#'
#' @md
#'
#' @section Representations:
#' * [QLearning]
#'
#' @export
#' @examples
#' alg = makeAlgorithm("qlearning")
makeAlgorithm = function(class, ...) {
  checkmate::assertChoice(class,
    c("qlearning"))#, "sarsa"))
  # fixme: check arguments here
  x = list(name = class, args = list(...))
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
