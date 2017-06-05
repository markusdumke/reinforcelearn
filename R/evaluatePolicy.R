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
evaluatePolicy = function(envir, policy, v = NULL, discount.factor = 1, precision = 0.0001) {
  
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  if (is.null(v)) {
    v = rep(0, envir$n.states)
  } else {
    checkmate::assertVector(v, len = envir$n.states)
  }
  non.terminal.states = setdiff(seq(0, envir$n.states - 1), envir$terminal.states)
  P = envir$transition.array
  improvement = TRUE
  
  # iterate while improvement in value function greater than epsilon for each element
  v2 = matrix(0, ncol = envir$n.actions, nrow = envir$n.states)
  while (improvement == TRUE) {
    for (i in seq_len(envir$n.actions)) {
      v2[non.terminal.states + 1, i] = policy[non.terminal.states + 1, i] * 
        (envir$reward.matrix[non.terminal.states + 1, i] + 
        discount.factor * P[non.terminal.states + 1, non.terminal.states + 1, i] %*% 
            v[non.terminal.states + 1])
    }
    v.new = rowSums(v2)
    improvement = any(abs(v - v.new) > precision)
    v = v.new
  }
  v
}
# vectorized, but slower version
# evaluatePolicy = function(envir, policy, v = NULL, discount.factor = 1, precision = 0.0001) {
#   
#   stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
#   if (is.null(v)) {
#     v = rep(0, envir$n.states)
#   } else {
#     checkmate::assertVector(v, len = envir$n.states)
#   }
#   v.new = v
#   P = matrix(envir$transition.array, nrow = envir$n.states, ncol = envir$n.actions * envir$n.states)
#   R = envir$reward.matrix
#   improvement = TRUE
#   
#   while (improvement == TRUE) {
#     v2 = Matrix::bdiag(v, v, v, v)
#     d = matrix(policy * (R + (P %*% v2)), nrow = envir$n.states, ncol = envir$n.actions)
#     v.new = rowSums(d)
#     improvement = any(abs(v - v.new) > precision)
#     v = v.new
#   }
#   v
# }
