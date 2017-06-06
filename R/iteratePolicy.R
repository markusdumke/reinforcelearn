#' Policy Iteration
#' 
#' Find optimal policy by dynamic programming. Iterate between evaluating a 
#' given policy and improving the policy by a greedy update. 
#' Converges to the optimal policy.
#' 
#' @details The algorithm runs until the policy does not change 
#' in two subsequent steps or if the 
#' specified number of iterations is exhausted.
#'
#' @inheritParams params
#'
#' @return a list with the optimal state value function (a numeric vector) 
#' and the optimal policy (a matrix of dimension: number of states x number of actions)
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @export
#' @seealso iterateValue
#' @examples
#' grid = gridworld$new()
#' Gridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix)
#' res = iteratePolicy(Gridworld1)
#' 
iteratePolicy <- function(envir, initial.policy = NULL, 
  discount.factor = 1, precision = 0.0001, iter = NULL) {
  
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  checkmate::assertNumber(discount.factor, lower = 0, upper = 1)
  checkmate::assertNumber(precision, lower = 0)
  checkmate::assertNumber(iter, null.ok = TRUE)
  
  if (is.null(initial.policy)) {
    initial.policy = matrix(1 / envir$n.actions, nrow = envir$n.states, 
      ncol = envir$n.actions)
  } else {
    if (envir$n.actions != ncol(initial.policy)) {
      stop("The number of columns of the policy must be equal to the number of actions.")
    }
    if (envir$n.states != nrow(initial.policy)) {
      stop("The number of rows of the policy must be equal to the number of states.")
    }
    if (any(rowSums(initial.policy) != 1)) {
      stop("The probabilities of each row of the policy must sum to 1.")
    }
  }
  
  non.terminal.states = setdiff(seq(0, envir$n.states - 1), envir$terminal.states)
  policy = initial.policy
  Q = policy
  v = rep(0, envir$n.states)
  j = 0
  
  while (TRUE) {
    if (!is.null(iter)) {
      j = j + 1
      if (j > iter) break
    }
    v = evaluatePolicy(envir, policy, v, discount.factor, precision)
    policy.old = policy
    for (i in seq_len(envir$n.actions)) {
      Q[non.terminal.states + 1, i] = envir$reward.matrix[non.terminal.states + 1, i] + 
        discount.factor * envir$transition.array[non.terminal.states + 1, non.terminal.states + 1, i] %*% 
        v[non.terminal.states + 1]
    }
    policy = improvePolicy(Q)
    if (identical(policy.old, policy)) break
  }
  
  list(v = v, policy = policy)
}
