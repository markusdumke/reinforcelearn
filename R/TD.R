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
#' @inheritParams documentParams
#'
#' @return [\code{numeric}] \cr
#'   Returns the state value function v
#' @export
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
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
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  checkmate::assertNumber(discount.factor, lower = 0, upper = 1)
  checkmate::assertNumber(learning.rate, lower = 0, upper = 1)
  checkmate::assertNumber(lambda, lower = 0, upper = 1)
  checkmate::assertInt(n.steps, lower = 1)
  if (envir$n.actions != ncol(policy)) {
    stop("The number of columns of the policy must be equal to the number of actions.")
  }
  if (envir$n.states != nrow(policy)) {
    stop("The number of rows of the policy must be equal to the number of states.")
  }
  if (any(rowSums(policy) != 1)) {
    stop("The probabilities of each row of the policy must sum to 1.")
  }
  
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
    td.target = envir$reward + discount.factor * v[envir$state + 1]
    td.error = td.target - v[state + 1]
    v = v + learning.rate * td.error * eligibility
    state = envir$state
    
    if (envir$done == TRUE) {
      envir$reset()
      state = envir$state
      eligibility = rep(0, n.states)
    }
  }
  
  v
}


# # rewards reward vector
# # v value function from state (S_t+n)
# compute_nstep_return = function(rewards, discount.factor, v) {
#   
#   sum(discount.factor ^ seq(0, length(rewards)) * c(rewards, v))
# }
# 
