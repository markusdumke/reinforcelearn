#' Value Iteration
#'
#' Find optimal policy by dynamic programming. Iterate between evaluating a
#' given policy (only one step), then improving the policy by a greedy update.
#' Converges to the optimal policy.
#' 
#' @details The algorithm runs until the improvement in the value 
#' function in two subsequent steps
#' is smaller than the given precision in all states or if the 
#' specified number of iterations is exhausted.
#'
#' @inheritParams params
#'
#' @return [\code{list(2)}] \cr
#'   Returns the optimal state value function [\code{numeric}] 
#'   and the optimal policy [\code{matrix}] (number of states x number of actions)
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @export
#' @seealso \code{\link{iteratePolicy}}
#' @examples
#' grid = makeEnvironment(transition.array = gridworld$transitions, 
#'   reward.matrix = gridworld$rewards)
#' res = iterateValue(grid)
#' 
iterateValue <- function(envir, v = NULL, discount.factor = 1, 
  precision = 0.0001, iter = NULL) {
  
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  if (is.null(v)) {
    v = rep(0, envir$n.states)
  } else {
    checkmate::assertNumeric(v, len = envir$n.states)
  }
  checkmate::assertNumber(discount.factor, lower = 0, upper = 1)
  checkmate::assertNumber(precision, lower = 0)
  checkmate::assertInt(iter, null.ok = TRUE)
  non.terminal.states = setdiff(seq(0, envir$n.states - 1), envir$terminal.states)
  P = envir$transition.array
  improvement = TRUE
  j = 0
  
  Q = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  while (improvement == TRUE) {
    if (!is.null(iter)) {
      j = j + 1
      if (j > iter) break
    }
    for (i in seq_len(envir$n.actions)) {
      Q[non.terminal.states + 1, i] = envir$reward.matrix[non.terminal.states + 1, i] + 
        discount.factor * P[non.terminal.states + 1, non.terminal.states + 1, i] %*% 
        v[non.terminal.states + 1]
    }
    v.new = apply(Q, 1, max)
    improvement = any(abs(v - v.new) > precision)
    v = v.new
  }
  policy = returnPolicy(Q, epsilon = 0)
  list(v = v, policy = policy)
}

