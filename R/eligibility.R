#' Eligibility traces
#'
#' Eligibility traces.
#'
#' Algorithms supporting eligibility traces:
#' * [QLearning]
#'
#' @param lambda \[`numeric(1)` in (0, 1)] \cr Trace decay parameter.
#' @param traces \[`character(1)`] \cr Type of eligibility trace update. One of `c("replace", "accumulate")`.
#'
#' @name Eligibility
#' @md
#'
#' @aliases eligibility
#'
#' @examples
#' alg = makeAlgorithm("qlearning", lambda = 0.8, traces = "accumulate")
NULL

Eligibility = R6::R6Class("Eligibility",
  public = list(
    lambda = 0,
    eligibility.type = NULL,
    E = NULL,
    initialize = function(lambda = 0, traces = "accumulate") {
      self$lambda = lambda
      if (traces == "replace") {
        self$eligibility.type = 1
      } else if (traces == "accumulate") {
        self$eligibility.type = 0
      }
    },
    reset = function(val.fun) {
      self$E = matrix(0, nrow = nrow(val.fun), ncol = ncol(val.fun))
    },
    increase = function(s, a) {
      self$E[s + 1L, a + 1L] = (1 - self$eligibility.type) * self$E[s + 1L, a + 1L] + 1
    },
    decrease = function(discount) {
      self$E = discount * self$lambda * self$E # sarsa
    }
  )
)
