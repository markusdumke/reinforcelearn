#' SARSA
#' 
#' Sarsa is an on-policy TD control algorithm to find the optimal policy.
#' The action value function Q will be updated 
#' towards the action value function of the next state S' and next action A' 
#' using an epsilon-greedy policy derived from Q.
#' The update formula is: 
#' \deqn{Q(S, A) <- Q(S, A) + \alpha[R + \gamma Q(S', A') - Q(S, A)]}
#'  
#' @inheritParams evaluatePolicy
#' @inheritParams predictMC
#' @param epsilon scalar numeric between 0 and 1: proportion of random samples 
#' in epsilon-greedy behaviour policy. Higher values of epsilon lead to more exploration.
#' @inheritParams td
#' @param seed scalar integer: random seed
#'
#' @importFrom stats runif
#' @importFrom nnet which.is.max
#' @return optimal action value function Q
#' @export
#' @references [Sutton and Barto (2017) page 138](https://webdocs.cs.ualberta.ca/~sutton/book/bookdraft2016sep.pdf#page=156)
#' 
#' @seealso [expectedSarsa]
#' @seealso [qlearning]
#' @examples 
#' grid = WindyGridworld$new()
#' Q = sarsa(grid, n.steps = 1000)$Q
sarsa <- function(envir, lambda = 0, n.steps = 100, alpha = 0.1, 
  epsilon = 0.1, discount.factor = 1, seed = NULL) {
  
  # input checking
  if (!is.null(seed)) set.seed(seed)
  
  time.steps.episode = c()
  
  n.states = envir$n.states
  n.actions = envir$n.actions
  Q = matrix(runif(n.states * n.actions), nrow = n.states, ncol = n.actions)
  Q[envir$terminal.states, ] = 0

  eligibility = matrix(0, nrow = n.states, ncol = n.actions)
  
  state = sample(envir$non.terminal.states, size = 1)
  action = sample_epsilon_greedy_action(Q[state, ], epsilon = epsilon)
  
  for (i in seq_len(n.steps)) {
    if (i %% 100 == 0) {
      print(paste("Step:", i))
    }
    
    envir$step(state, action)
    
    next.action = sample_epsilon_greedy_action(Q[state, ], epsilon = epsilon)
    
    indicator = matrix(0, nrow = n.states, ncol = n.actions)
    indicator[state, action] = 1
    
    eligibility = discount.factor * lambda * eligibility + indicator
    TD.target = envir$reward + discount.factor * Q[envir$next.state, next.action]
    TD.error = TD.target - Q[state, action]
    Q = Q + alpha * TD.error * eligibility
    
    state = envir$next.state
    action = next.action
    
    if (envir$episode.over == TRUE) {
      time.steps.episode = append(time.steps.episode, i)
      envir$setEpisodeOverFalse()
      state = sample(envir$non.terminal.states, size = 1)
      action = sample_epsilon_greedy_action(Q[state, ], epsilon = epsilon)
      eligibility = matrix(0, nrow = n.states, ncol = n.actions)
    }
  }
  
  list(Q = Q, time.steps.episode = time.steps.episode)
}

# Q a numeric vector: the action value function for a given state
# epsilon numeric scalar in [0, 1]: probability of selecting a random action
# sample_epsilon_greedy_action(c(1, 2, 3), epsilon = 0.2)
sample_epsilon_greedy_action <- function(Q, epsilon) {
  
  greedy_action = argmax(Q)
  random_actions = seq(1, length(Q))
  # non_greedy_actions = actions[actions != greedy_action]
  action = sample(c(greedy_action, random_actions), size = 1,  
    prob = c(1 - epsilon, rep(epsilon / length(random_actions), length(random_actions))))
  
  action
}

# Argmax (ties broken randomly)
# x numeric matrix or numeric vector
argmax <- function(x) {
  nnet::which.is.max(x)
}

# plot(y = seq_along(Q$episodes.over), x = Q$episodes.over, type = "l")
# plot(x = seq_along(Q$episodes.over), y = diff(c(0, Q$episodes.over)), type = "l", 
#   xlim = c(0, 200), ylab = "Episode length", xlab = "Episode")
