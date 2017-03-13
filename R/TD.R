#' Temporal difference learning
#' 
#' Temporal difference (TD) learning is a method to estimate the state value 
#' function for a given policy. 
#' It works by sampling one step from the 
#' environment and plugging this into the update equation (Bootstrapping). This also
#' works for non-episodic environments.
#' 
#' The implementation works with eligibility traces. Eligibility traces combine 
#' a frequency and recency heuristic. Whenever a state is visited, 
#' the eligibility of this state is increased. Over time the eligibility decreases 
#' exponentially. This way, states that occured often and recently get most 
#' credit for a reward and therefore are updated more strongly than states 
#' observed infrequently and longer time ago.
#'
#' @inheritParams evaluatePolicy
#' @inheritParams predictMC
#' @param lambda scalar numeric in (0, 1): Then lambda = 0 only current state 
#' is updated (this is equivalent to TD(0)), for lambda = 1 all states visited 
#' are updated, this is roughly equivalent to every-visit Monte Carlo.
#' @param n.steps integer scalar: number of evaluations (steps in the environment)
#'
#' @return state value function v
#' @export
#' @seealso [sarsa]
#' @examples 
#' set.seed(1477)
#' # Define environment, here simple gridworld
#' grid = gridworld$new()
#' 
#' # Define random policy
#' random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
#'   ncol = grid$n.actions)
#' 
#' # Estimate state value function with temporal-difference learning (TD(0))
#' v = td(random.policy, grid, lambda = 0)
td = function(policy, envir, lambda = 0, n.steps = 100, 
  discount.factor = 1, alpha = 0.1) {
  
  # input checking
  
  n.states = envir$n.states
  v = rep(0, n.states)
  eligibility = rep(0, n.states) # keep an eligibility value for each state
  state = sample(envir$non.terminal.states, size = 1)
  
  for (i in seq_len(n.steps)) {
    if (i %% 100 == 0) {
      print(paste("Step:", i))
    }
    
    action = sample(envir$actions, prob = policy[state, ], size = 1)
    envir$step(state, action)
    
    indicator = rep(0, n.states)
    indicator[state] = 1
    
    eligibility = discount.factor * lambda * eligibility + indicator
    TD.error = envir$reward + discount.factor * v[envir$next.state] - v[state]
    v = v + alpha * TD.error * eligibility
    state = envir$next.state
    
    if (envir$episode.over == TRUE) {
      envir$setEpisodeOverFalse()
      state = sample(envir$non.terminal.states, size = 1)
      eligibility = rep(0, n.states)
    }
  }

  v
}


# rewards reward vector
# v value function from state (S_t+n)
compute_nstep_return = function(rewards, discount.factor, v) {
  
  sum(discount.factor ^ seq(0, length(rewards)) * c(rewards, v))
}

