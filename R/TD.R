#' N-step Temporal difference learning
#' 
#' Temporal difference (TD) learning works by sampling one step from the 
#' environment and plugging this into the update equation (Bootstrapping). This
#'  works also for non-episodic environments.
#'  
#' Currently implemented: n-step TD -> next: Implement TD(lambda)
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
#' v = TD(random.policy, grid, n = 1, n.episodes = 100, alpha = 0.1) # not working?
TD = function(policy, envir, n.episodes = 1, n = 10, 
  discount.factor = 1, alpha = NULL) {
  
  # # input checking
  # 
  # n.states = nrow(policy)
  # # n.visits = rep(0, n.states)
  # v = rep(0, n.states)
  # state = sample(envir$non.terminal.states, 1)
  # 
  # states = rep(0, n + 1)
  # rewards = rep(0, n + 1)
  # 
  # # parallelize episodes?
  # for (i in seq_len(n.episodes)) {
  #   # if(i %% 100 == 0) alpha = alpha / i
  #   # initialize s
  #   states[1] = sample(envir$non.terminal.states, 1)
  #   
  #   time.steps = Inf
  #   t = 0
  #   
  #   while (t < time.steps) {
  #     
  #     t = t + 1
  #     
  #     # sample action from policy pi(.|St)
  #     action = sample(envir$actions, prob = policy[state, ], size = 1)
  #     
  #     # observe and store next reward and next state
  #     envir$step(states[t], action)
  #     states[t + 1] = envir$next.state
  #     rewards[t + 1] = envir$reward
  #     
  #     # if St+1 terminal then time.steps = t + 1
  #     if (envir$episode.over == TRUE) {
  #       envir$setEpisodeOverFalse()
  #       time.steps = t + 1
  #     }
  #     
  #     # which state's value function gets updated
  #     tau = t - n + 1
  #     
  #     
  #     if (tau >= 1) {
  #       sequ = seq(tau, min(tau + n - 1, time.steps))
  #       G = sum(discount.factor ^ sequ * rewards[sequ + 1])
  #       if ( (tau + n) < time.steps) {
  #         G = G + discount.factor ^ n * v[states[tau + n]]
  #       }
  #       v[states[tau]] = v[states[tau]] + alpha * (G - v[states[tau]])
  #     }
  #     
  #     if (tau == time.steps - 1) break
  #   }
  #   
  # }
  # 
  # v
  
  # states, actions and rewards of n-steps must be saved, 
  #   then states can be updated
  
  alpha_input <- alpha
  
  n.states = nrow(policy)
  n.visits = rep(0, n.states)
  v = rep(0, n.states)
  possible.states = envir$non.terminal.states
  
  for (i in seq_len(n.episodes)) {
    if (i %% 50 == 0) {
      print(paste("Episode:", i))
    }
    envir$setEpisodeOverFalse()
    initial.state = sample(possible.states, 1)
    episode = sampleEpisode(policy, envir, initial.state)
    episode.len = length(episode$states)
    
    for (j in (seq_len(episode.len - 1))) {
      state = episode$states[j]
      n.visits[state] = n.visits[state] + 1
      nsteps.ahead = j + n
      if (nsteps.ahead > episode.len) {
        nsteps.ahead = episode.len
      }
      
      rewards = episode$rewards[seq(j + 1, nsteps.ahead)]
      state.nsteps.ahead = episode$states[nsteps.ahead]
      
      return = compute_nstep_return(rewards, discount.factor, v[state.nsteps.ahead])
      td_error = return - v[state]
      
      if (is.null(alpha_input)) {
        alpha = 1 / n.visits[state]
      }
      
      v[state] = v[state] + alpha * td_error
    }
    
    
    # action = sample(envir$actions, prob = policy[state, ], size = 1)
    # envir$step(state, action)
    #   td_error = envir$reward + discount.factor * v[envir$next.state] - v[state]
    #   if (is.null(alpha_input)) {
    #     alpha = 1 / n.visits[state]
    #   }
    #   v[state] = v[state] + alpha * td_error
    #   state = envir$next.state
  }
  
  return(v)
}


# rewards reward vector
# v value function from state (S_t+n)
compute_nstep_return = function(rewards, discount.factor, v) {
  
  sum(discount.factor ^ seq(0, length(rewards)) * c(rewards, v))
}
