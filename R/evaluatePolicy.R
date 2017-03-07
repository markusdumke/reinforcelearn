#' Policy Evaluation
#'
#' Evaluate a given policy in an environment.
#'
#' @details The algorithm runs until the improvement in the value function in two subsequent steps
#' is smaller than epsilon.
#' @param policy a policy specified as a probability matrix (states x actions)
#' @param envir the environment, an R6 class. See also [envir()].
#' @param discount.factor scalar numeric, discounting future rewards
#' @param epsilon scalar numeric, algorithm stops when improvement is smaller than epsilon
#'
#' @return the state value function v
#'
#' @export
#' @examples
#' # Define uniform random policy, take each action with probability 0.25
#' grid = gridworld$new()
#' random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
#'   ncol = grid$n.actions)
#' 
#' # Evaluate given policy for gridworld example
#' v = evaluatePolicy(random.policy, grid)
#' 
evaluatePolicy = function(policy, envir, discount.factor = 1, epsilon = 0.0001) {
  n.states = envir$n.states

  # initialize v to zeros
  v = rep(0, n.states)
  v.new = v
  terminal.states = envir$terminal.states
  not.terminal.states = setdiff(seq(1, n.states), terminal.states)

  P = envir$transition.array
  reward.t = t(envir$reward.matrix)
  improvement = TRUE

  # iterate while improvement in value function greater than epsilon for each element
  while (improvement == TRUE) {
    for (state in not.terminal.states) {
      v.new[state] = policy[state, , drop = FALSE] %*%
        (reward.t[, state, drop = FALSE] + discount.factor * t(P[state, , ]) %*%
           as.matrix(v, nrow = 16))
      improvement = any(abs(v - v.new) > epsilon)
    }
    v = v.new
  }
  return(v)
}
