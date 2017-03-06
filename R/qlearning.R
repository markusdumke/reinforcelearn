#' Q-Learning
#' 
#' Off-policy TD control algorithm. Q-Learning finds the optimal action value
#' function Q independent of the policy followed. Using an epsilon-greedy
#' behaviour policy states and actions are sampled. Given a state-action pair 
#' the optimal next action is considered by taking the max over all possible 
#' successor action values.
#'  
#' Under the assumption that all state-action pairs are visited (which is 
#' achieved using a stochastic epsilon-greedy policy) Q-Learning converges to 
#' the optimal action value function Q*. The update formula is: 
#' \deqn{Q(S, A) <- Q(S, A) + \alpha[R + \gamma max_a Q(S', a) - Q(S, A)]}
#'
#' @inheritParams evaluatePolicy
#' @inheritParams predictMC
#' @inheritParams sarsa
#'
#' @return optimal action value function
#' @seealso [dqlearning()]
#' @references [Sutton and Barto (2017) page 140](https://webdocs.cs.ualberta.ca/~sutton/book/bookdraft2016sep.pdf#page=158)
#' @export
#' @examples
#' grid = gridworld$new()
#' Q = qlearning(grid, n.episodes = 1000)
qlearning <- function(envir, n.episodes = 10, alpha = 0.1, 
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
      action = sample_epsilon_greedy_action(Q[state, ], epsilon = 0.1)
      
      envir$step(state, action)
      next.state = envir$next.state
      reward = envir$reward
      
      # update Q for visited state-action pair maximizing over next state
      TD.target = reward + discount.factor * max(Q[next.state, ]) 
      TD.error = TD.target - Q[state, action] 
      Q[state, action] = Q[state, action] + alpha * TD.error
      state = next.state
    }
  }
  
  Q
}
