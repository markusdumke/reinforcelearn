#' Temporal difference learning
#' 
#' Temporal difference (TD) learning works by sampling one step from the 
#' environment and plugging this into the update equation (Bootstrapping). This
#'  works also for non-episodic environments.
#'  
#'  Currently implemented: n-step TD (forward-view)
#'
#' @inheritParams evaluatePolicy
#' @inheritParams predictMC
#' @param n scalar integer: number of steps TD target looks into the 
#' future
#'
#' @return value function
#' @export
#' @seealso [predictMC]
#' @examples 
#' set.seed(1477)
#' # Define environment, here simple gridworld
#' grid = gridworld$new()
#' 
#' # Define random policy
#' random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
#'   ncol = grid$n.actions)
#' 
#' # Estimate state value function with temporal-difference learning
#' v = TD(random.policy, grid, n = 2, alpha = 0.1)
TD = function(policy, envir, n.episodes = 1, n = 10, 
  discount.factor = 1, alpha = 0.1) {
  
  # input checking
  # alpha_input = alpha
  # check_number(discount.factor, lower = 0, upper = 1)
  # if (!is.null(alpha)) {
  #   check_number(alpha, lower = 0, upper = 1)
  # }
  
  n.states = nrow(policy)
  # n.visits = rep(0, n.states)
  v = rep(0, n.states)
  possible.states = envir$states[envir$states != envir$terminal.states]
  state = sample(possible.states, 1)
  
  states = rep(0, n + 1)
  rewards = rep(0, n + 1)
  
  # parallelize episodes
  for (i in seq_len(n.episodes)) {
    
    # initialize s
    states[1] = sample(possible.states, 1)
    
    time.steps = Inf
    t = 0
    
    while (t < time.steps) {
      
      t = t + 1
      
      # sample action from policy pi(.|St)
      action = sample(envir$actions, prob = policy[state, ], size = 1)
      
      # observe and store next reward and next state
      envir$step(states[t], action)
      states[t + 1] = envir$next.state
      rewards[t + 1] = envir$reward
      
      # if St+1 terminal then time.steps = t + 1
      if (envir$episode.over == TRUE) {
        envir$setEpisodeOverFalse()
        time.steps = t + 1
      }
      
      # which state's value function gets updated
      tau = t - n + 1
      
      
      if (tau >= 1) {
        sequ = seq(tau, min(tau + n - 1, time.steps))
        G = sum(discount.factor ^ sequ * rewards[sequ + 1])
        if ( (tau + n) < time.steps) {
          G = G + discount.factor ^ n * v[states[tau + n]]
        }
        v[states[tau]] = v[states[tau]] + alpha * (G - v[states[tau]])
      }
      
      if (tau == time.steps - 1) break
    }
    
  }
  
  v
  
  
  
  
  
  
  
  
  
  # n.states = nrow(policy)
  # n.visits = rep(0, n.states)
  # v = rep(0, n.states)
  # possible.states = envir$states[envir$states != envir$terminal.states]
  # state = sample(possible.states, 1)
  # 
  # states = rep(0, iter)
  # 
  # for (i in seq_len(iter)) {
  #   if (i %% 100 == 0) {
  #     print(paste("Iteration:", i))
  #   }
  #   if (envir$episode.over == TRUE) {
  #     envir$setEpisodeOverFalse()
  #     state = sample(possible.states, 1)
  #   }
  #   n.visits[state] = n.visits[state] + 1
  #   action = sample(envir$actions, prob = policy[state, ], size = 1)
  #   envir$step(state, action)
  #   td_error = envir$reward + discount.factor * v[envir$next.state] - v[state]
  #   if (is.null(alpha_input)) {
  #     alpha = 1 / n.visits[state]
  #   }
  #   v[state] = v[state] + alpha * td_error
  #   state = envir$next.state
  # }
  # 
  # return(v)
}


# rewards reward vector
# v value function from state (S_t+n)
compute_nstep_return = function(rewards, discount.factor, v) {
  
  sum(discount.factor ^ seq(0, length(rewards) - 1) * c(rewards, v))
}
