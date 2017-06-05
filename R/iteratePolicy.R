#' Policy Iteration
#' 
#' Find optimal policy by dynamic programming. Iterate between evaluating a 
#' given policy (until convergence) and improving the policy by a greedy update. 
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
  v = rep(0, envir$n.states)
  
  while (TRUE) {
    v = evaluatePolicy(envir, policy, v, discount.factor, precision)
    policy.old = policy
    policy = improvePolicy(v, envir, discount.factor)
    if (identical(policy.old, policy)) break
  }
  
  list(v = v, policy = policy)
}
