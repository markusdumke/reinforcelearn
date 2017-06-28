#' Temporal difference learning
#' 
#' Temporal difference (TD) learning is a method to estimate the state value 
#' function for a given policy. It works by sampling one step from the 
#' environment and plugging this into the update equation (Bootstrapping). This
#' also works for non-episodic environments.
#' 
#' The implementation works with eligibility traces. Eligibility traces combine 
#' a frequency and recency heuristic. Whenever a state is visited, the
#' eligibility of this state is increased. Over time the eligibility decreases 
#' exponentially. This way, states that occured often and recently get most 
#' credit for a reward and therefore are updated more strongly than states 
#' observed infrequently and longer time ago.
#'
#' @inheritParams params
#'
#' @return state value function v
#' @export
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @seealso [sarsa]
#' @examples 
#' # Define environment, here simple gridworld
#' grid = makeEnvironment(transition.array = gridworld$transitions, 
#'   reward.matrix = gridworld$rewards)
#'   
#' # Define random policy
#' random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
#'   ncol = grid$n.actions)
#' 
#' # Estimate state value function with temporal-difference learning (TD(0))
#' v = td(grid, random.policy, lambda = 0, n.steps = 1000)
#' print(round(matrix(v, ncol = 4, byrow = TRUE)))
td = function(envir, policy, lambda = 0, n.steps = 100, 
  discount.factor = 1, learning.rate = 0.1) {
  
  # input checking
  
  # exact learning.rate version?
  
  n.states = envir$n.states
  v = rep(0, n.states)
  eligibility = rep(0, n.states) # keep an eligibility value for each state
  envir$reset()
  state = envir$state
  
  for (i in seq_len(n.steps)) {
    if (i %% 1000 == 0) {
      # learning.rate = learning.rate / 2
      print(paste("Step:", i))
    }
    
    action = sample(envir$actions, prob = policy[state + 1, ], size = 1)
    envir$step(action)
    
    indicator = rep(0, n.states)
    indicator[state + 1] = 1
    
    eligibility = discount.factor * lambda * eligibility + indicator
    TD.target = envir$reward + discount.factor * v[envir$state + 1]
    TD.error = TD.target - v[state + 1]
    v = v + learning.rate * TD.error * eligibility
    state = envir$state
    
    if (envir$episode.over == TRUE) {
      envir$reset()
      state = envir$state
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

