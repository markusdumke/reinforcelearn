#' Double Q-Learning
#' 
#' The idea behind Double Q-Learning is to have two separate action value 
#' functions Q1 and Q2. Actions are chosen from an epsilon-greedy policy derived 
#' from Q1 + Q2. With equal probability one of Q1 and Q2 is then updated 
#' following the same update rule as in Q-Learning. This avoids maximization bias.
#'
#' @inheritParams sarsa
#' @inheritParams evaluatePolicy
#' @inheritParams predictMC
#'
#' @return optimal value function Q
#' @export
#' @references Sutton & Barto (page 145)
#' @seealso qlearning
#'
#' @examples
#' grid = gridworld$new()
#' # Q = dqlearning(grid, n.episodes = 1000) # not working?
dqlearning <- function(envir, n.episodes = 10, alpha = 0.1, epsilon = 0.1, 
  discount.factor = 1, seed = NULL) {
  
  # input checking
  if (!is.null(seed)) set.seed(seed)
  
  # initialize Q1 and Q2 randomly
  n.states = envir$n.states
  n.actions = envir$n.actions
  Q1 = matrix(runif(n.states * n.actions), nrow = n.states, ncol = n.actions)
  Q1[envir$terminal.states, ] = 0
  
  Q2 = matrix(runif(n.states * n.actions), nrow = n.states, ncol = n.actions)
  Q2[envir$terminal.states, ] = 0
  
  for (i in seq_len(n.episodes)) {
    
    # initialize starting state s
    state = sample(envir$non.terminal.states, size = 1)
    
    while (state %in% envir$non.terminal.states) { # replace by episode.over?
      # epsilon-greedy policy, sample action
      action = sample_epsilon_greedy_action(Q1[state, ] + Q2[state, ], epsilon = epsilon)
      
      envir$step(state, action)
      next.state = envir$next.state
      reward = envir$reward
      
      # update Q for visited state-action pair maximizing over next state
      which_q <- sample(c("Q1", "Q2"), size = 1)
      if (which_q == "Q1") {
        update_q(Q1, Q2, state, action, next.state, reward, discount.factor, alpha)
      } else {
        update_q(Q2, Q1, state, action, next.state, reward, discount.factor, alpha)
      }
      
      state = next.state
    }
  }
  
  list(Q1 = Q1, Q2 = Q2)
}

update_q <- function(Q1, Q2, state, action, next.state, reward, 
  discount.factor, alpha) {
  
  TD.target = reward + discount.factor * Q2[next.state, argmax(Q1[next.state])]
  TD.error = TD.target - Q1[state, action] 
  Q1[state, action] = Q1[state, action] + alpha * TD.error
  Q1
}
