#' Value Iteration
#'
#' Find optimal policy by dynamic programming. Iterate between evaluating a
#' given policy (only one step), then improving the policy by a greedy update.
#' Converges to the optimal policy.
#'
#' @inheritParams params
#'
#' @return a list with the optimal state value function and optimal policy
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @export
#' @examples
#' grid = gridworld$new()
#' Gridworld1 = makeEnvironment(transition.array = grid$transition.array,
#'   reward.matrix = grid$reward.matrix)
#' res = iterateValue(Gridworld1)
#' 
iterateValue <- function(envir, v = NULL, discount.factor = 1, precision = 0.0001) {
  
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  if (is.null(v)) {
    v = rep(0, envir$n.states)
  } else {
    checkmate::assertVector(v, len = envir$n.states)
  }
  non.terminal.states = setdiff(seq(0, envir$n.states - 1), envir$terminal.states)
  P = envir$transition.array
  improvement = TRUE
  
  Q = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  while (improvement == TRUE) {
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

