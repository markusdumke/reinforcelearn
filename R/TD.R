#' Temporal difference learning
#' 
#' Temporal difference (TD) learning works by sampling one step from the 
#' environment and plugging this into the update equation (Bootstrapping). This
#'  works also for non-episodic environments.
#'  
#'  Currently implemented: TD(0)
#'
#' @inheritParams evaluatePolicy
#' @inheritParams predictMC
#' @param n.steps scalar integer: number of steps
#'
#' @return value function
#' @export
#' @seealso [predictMC]
#' @examples 
#' set.seed(1477)
#' grid = gridworld_R6$new()
#' 
#' # Define random policy
#' n.states = nrow(grid$reward.matrix)
#' n.actions = ncol(grid$reward.matrix)
#' random.policy = matrix(1 / n.actions, nrow = n.states, ncol = n.actions)
#' 
#' # Estimate state value function with temporal-difference learning
#' v = TD(random.policy, grid, n.steps = 100, alpha = 0.1)
TD = function(policy, envir, n.steps = 100, discount.factor = 1, alpha = NULL) {
  
  alpha_input = alpha
  check_number(discount.factor, lower = 0, upper = 1)
  if (!is.null(alpha)) {
    check_number(alpha, lower = 0, upper = 1)
  }
  
  n.states = nrow(policy)
  n.visits = rep(0, n.states)
  v = rep(0, n.states)
  possible.states = envir$states[envir$states != envir$terminal.states]
  state = sample(possible.states, 1)
  
  for (i in seq_len(n.steps)) {
    if (i %% 100 == 0) {
      print(paste("Step:", i))
    }
    if (envir$episode.over == TRUE) {
      envir$setEpisodeOverFalse()
      state = sample(possible.states, 1)
    }
    n.visits[state] = n.visits[state] + 1
    action = sample(envir$actions, prob = policy[state, ], size = 1)
    envir$step(state, action)
    td_error = envir$reward + discount.factor * v[envir$next.state] - v[state]
    if (is.null(alpha_input)) {
      alpha = 1 / n.visits[state]
    }
    v[state] = v[state] + alpha * td_error
    state = envir$next.state
  }
  
  return(v)
}
