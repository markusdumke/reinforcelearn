#' SARSA(lambda)
#' 
#' Currently implemented: Sarsa(0). Sarsa is an on-policy TD control algorithm. 
#' Action value function Q will be updated 
#' towards the action value function of the next state and next action using an 
#' epsilon-greedy policy derived from Q.
#'  
#' @inheritParams evaluatePolicy
#' @inheritParams predictMC
#' @param epsilon scalar numeric between 0 and 1: proportion of random samples 
#' in epsilon-greedy behaviour policy. The higher epsilon the more exploration.
#' @param lambda scalar integer between 0 and 1
#' @param seed scalar integer: random seed
#'
#' @importFrom stats runif
#' @return optimal action value function Q
#' @export
#' @references [Sutton and Barto (2017) page 138](https://webdocs.cs.ualberta.ca/~sutton/book/bookdraft2016sep.pdf#page=156)

#' @seealso [expectedSarsa()]
#' @seealso [qlearning()]
#' @examples 
#' grid = gridworld$new()
#' Q = sarsa(grid, n.episodes = 1000)
sarsa <- function(envir, lambda = NULL, n.episodes = 10, alpha = 0.1, epsilon = 0.1,
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
    
    # epsilon-greedy policy, sample action
    action = sample_epsilon_greedy_action(Q[state, ], epsilon = epsilon)
    
    while (state %in% envir$non.terminal.states) {
      envir$step(state, action)
      next.state = envir$next.state
      reward = envir$reward
      next.action = sample_epsilon_greedy_action(Q[state, ], epsilon = epsilon)
      
      # update Q for visited state-action pair using one-step lookahead
      TD.target = reward + discount.factor * Q[next.state, next.action] 
      TD.error = TD.target - Q[state, action] 
      Q[state, action] = Q[state, action] + alpha * TD.error
      state = next.state
      action = next.action
    }
  }
  
  Q
}

# Q a numeric vector: the action value function for a given state
# epsilon numeric scalar in [0, 1]: probability of selecting a random action
# sample_epsilon_greedy_action(c(1, 2, 3), epsilon = 0.2)
sample_epsilon_greedy_action <- function(Q, epsilon) {
  
  greedy_action = which.max(Q)
  random_actions = seq(1, length(Q))
  # non_greedy_actions = actions[actions != greedy_action]
  action = sample(c(greedy_action, random_actions), size = 1,  
    prob = c(1 - epsilon, rep(epsilon / length(random_actions), length(random_actions))))
  
  action
}

# Argmax (ties broken randomly)
# x numerical matrix
argmax <- function(x) {
  max.col(x)
}
