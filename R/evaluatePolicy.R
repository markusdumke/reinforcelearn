#' Policy Evaluation (Dynamic Programming)
#'
#' Evaluate a given policy in an environment using dynamic programming. 
#' 
#' With the Bellmann equation the update
#' \deqn{v(s) <- \sum \pi(a|s) (R + \gamma \sum Pss' v(s')])}
#' 
#' @details The algorithm runs until the improvement in the value function in two subsequent steps
#' is smaller than epsilon.
#' @param policy numeric matrix: a policy specified as a probability matrix (states x actions)
#' @param envir an R6 class: the reinforcement learning environment created by [makeEnvironment].
#' @param discount.factor scalar numeric, discounting future rewards
#' @param epsilon scalar numeric, algorithm stops when improvement is smaller than epsilon
#'
#' @return the state value function v, a numeric vector
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
  v
}
