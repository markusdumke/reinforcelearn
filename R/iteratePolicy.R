#' Policy Iteration
#' 
#' Find optimal policy by dynamic programming. Iterate between evaluating a 
#' given policy (until convergence) and improving the policy by a greedy update. 
#' Converges to the optimal policy.
#'
#' @inheritParams params
#'
#' @return the optimal state value function and optimal policy
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @export
#' @examples
#' grid = gridworld$new()
#' Gridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix)
#' res = iteratePolicy(Gridworld1)
#' 
iteratePolicy <- function(envir, initial.policy = NULL, 
  discount.factor = 1, precision = 0.0001) {
  
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  if (is.null(initial.policy)) {
    initial.policy = matrix(1 / envir$n.actions, nrow = envir$n.states, 
      ncol = envir$n.actions)
  }
  
  policy = initial.policy
  n.states = envir$n.states
  v = rep(0, n.states)
  non.terminal.states = setdiff(seq(0, n.states - 1), envir$terminal.states)
  P = envir$transition.array
  reward.t = t(envir$reward.matrix)
  
  while (TRUE) {
    v = evaluatePolicy2(policy, P, reward.t, v, non.terminal.states, 
      discount.factor, precision)
    policy.old = policy
    policy = improvePolicy(v, envir, discount.factor)
    if (identical(policy.old, policy)) break
  }
  
  list(v = v, policy = policy)
}


improvePolicy = function(v, envir, discount.factor) {
  # multiply each transition matrix for each action P[, ,  i] 
  #   with reward plus discounted value of next state
  Q = apply(envir$transition.array, 3, function(x) 
    rowSums(x %*% (envir$reward.matrix + discount.factor * v)))
  greedy.actions = apply(Q, 1, argmax)
  policy = matrix(0, nrow = nrow(Q), ncol = ncol(Q))
  policy[matrix(c(seq_len(envir$n.states), greedy.actions), ncol = 2)] = 1
  policy
}
