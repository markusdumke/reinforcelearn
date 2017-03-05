#' Expected Sarsa
#' 
#' Expected Sarsa is similar to Q-Learning but instead of taking the max over 
#' all possible next actions, an average is computed.
#'
#' @inheritParams evaluatePolicy
#' @inheritParams predictMC
#' @inheritParams sarsa
#' @param epsilon scalar numeric between 0 and 1: proportion of random samples 
#' in epsilon-greedy behaviour policy. The higher epsilon the more exploration.
#'
#' @return optimal action value function Q
#' @export
#' @seealso sarsa
#' @seealso qlearning
#' @references Sutton and Barto (2017) page 142
#' @examples 
#' grid = gridworld$new()
#' # Q = expectedSarsa(grid, n.episodes = 1000) not working
expectedSarsa <- function(envir, n.episodes = 10, alpha = 0.1, epsilon = 0.1, 
  discount.factor = 1, seed = NULL) {
  
   # input checking
  if (!is.null(seed)) set.seed(seed)
  
  # initialize Q randomly
  n.states = envir$n.states
  n.actions = envir$n.actions
  Q = matrix(runif(n.states * n.actions), nrow = n.states, ncol = n.actions)
  Q[envir$terminal.states, ] = 0
  
  for (i in seq_len(n.episodes)) {
    
    # initialize starting state s
    state = sample(envir$non.terminal.states, size = 1)
    
    while (state %in% envir$non.terminal.states) { # replace by episode.over?
      # epsilon-greedy policy, sample action
      action = sample_epsilon_greedy_action(Q[state, ], epsilon = epsilon)
      policy = make_epsilon_greedy_policy(Q, epsilon = epsilon)
      
      envir$step(state, action)
      next.state = envir$next.state
      reward = envir$reward
      
      # update Q for visited state-action pair averaging over next actions
      TD.target = reward + discount.factor * sum(policy[next.state, ] * Q[next.state, ])
      TD.error = TD.target - Q[state, action] 
      Q[state, action] = Q[state, action] + alpha * TD.error
      state = next.state
    }
  }
  
  Q
}

# return epsilon-greedy policy
make_epsilon_greedy_policy <- function(Q, epsilon) {
  policy = matrix(0, nrow = nrow(Q), ncol = ncol(Q))
  index <- matrix(c(1, 2, max.col(Q)), ncol = 2)
  policy[index] = 1 - epsilon
  policy = policy + epsilon / ncol(Q)
  policy
}
