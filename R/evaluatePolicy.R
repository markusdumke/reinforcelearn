#' Policy Evaluation (Dynamic Programming)
#'
#' Evaluate a given policy in an environment using dynamic programming. 
#' 
#' With the Bellmann equation update
#' \deqn{v(s) <- \sum \pi(a|s) (R + \gamma \sum Pss' v(s')])}
#' 
#' @details The algorithm runs until the improvement in the value 
#' function in two subsequent steps
#' is smaller than the given precision for all states.
#' 
#' @inheritParams params
#'
#' @return the state value function v, a numeric vector
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @export
#' @importFrom Matrix bdiag
#' @examples
#' # Define uniform random policy, take each action with equal probability
#' grid = gridworld$new()
#' Gridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix)
#' random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
#'   ncol = grid$n.actions)
#' 
#' # Evaluate given policy for gridworld example
#' v = evaluatePolicy(Gridworld1, random.policy)
#' print(round(matrix(v, ncol = 4, byrow = TRUE)))
#' 
evaluatePolicy = function(envir, policy, discount.factor = 1, precision = 0.0001) {
  
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  v = rep(0, envir$n.states)
  v.new = v
  P = matrix(envir$transition.array, nrow = envir$n.states, ncol = envir$n.actions * envir$n.states)
  R = envir$reward.matrix
  improvement = TRUE
  
  while (improvement == TRUE) {
    v2 = Matrix::bdiag(v, v, v, v)
    d = matrix(policy * (R + (P %*% v2)), nrow = 16, ncol = 4)
    v.new = rowSums(d)
    improvement = any(abs(v - v.new) > precision)
    v = v.new
  }
  v
}
