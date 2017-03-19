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
#' @param psi scalar numeric, algorithm stops when improvement is smaller than psi
#'
#' @return the state value function v, a numeric vector
#'
#' @export
#' @examples
#' # Define uniform random policy, take each action with probability 0.25
#' grid = gridworld$new()
#' Gridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix, terminal.states = grid$terminal.states,
#'   initial.state = grid$initial.state)
#' random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
#'   ncol = grid$n.actions)
#' 
#' # Evaluate given policy for gridworld example
#' v = evaluatePolicy(random.policy, grid)
#' 
evaluatePolicy = function(policy, envir, discount.factor = 1, psi = 0.0001) {
  
  n.states = envir$n.states
  v = rep(0, n.states)
  v.new = v
  non.terminal.states = setdiff(seq(0, n.states - 1), envir$terminal.states)

  P = envir$transition.array
  reward.t = t(envir$reward.matrix)
  improvement = TRUE

  # iterate while improvement in value function greater than epsilon for each element
  while (improvement == TRUE) {
    for (state in non.terminal.states) {
      v.new[state + 1] = policy[state + 1, , drop = FALSE] %*%
        (reward.t[, state + 1, drop = FALSE] + discount.factor * t(P[state + 1, , ]) %*%
           as.matrix(v, nrow = 16))
      improvement = any(abs(v - v.new) > psi)
    }
    v = v.new
  }
  v
}
