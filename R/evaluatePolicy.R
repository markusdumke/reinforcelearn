#' Policy Evaluation (Dynamic Programming)
#'
#' Evaluate a given policy in an environment using dynamic programming. 
#' 
#' With the Bellmann equation update
#' \deqn{v(s) <- \sum \pi(a|s) (R + \gamma \sum Pss' v(s')])}
#' 
#' @details The algorithm runs until the improvement in the value 
#' function in two subsequent steps
#' is smaller than precision.
#' 
#' @inheritParams params
#'
#' @return the state value function v, a numeric vector
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @export
#' @examples
#' # Define uniform random policy, take each action with probability 0.25
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
  
  n.states = envir$n.states
  v = rep(0, n.states)
  v.new = v
  non.terminal.states = setdiff(seq(0, n.states - 1), envir$terminal.states)
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
