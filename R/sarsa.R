#' SARSA (Table-lookup)
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
#' in epsilon-greedy behaviour policy. Higher values of epsilon lead to more 
#' exploration.
#' @inheritParams td
#' @param seed scalar integer: random seed
#'
#' @importFrom stats runif
#' @importFrom nnet which.is.max
#' @return optimal action value function Q
#' @export
#' @references [Sutton and Barto (2017) page 138](https://webdocs.cs.ualberta.ca/~sutton/book/bookdraft2016sep.pdf#page=156)
#' @seealso [td]
#' @seealso [expectedSarsa]
#' @seealso [qlearning]
#' @examples 
#' grid = WindyGridworld$new()
#' WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix, 
#'   terminal.states = grid$terminal.states, 
#'   initial.state = grid$initial.state)
#' res = sarsa(WindyGridworld1, n.steps = 1000)
#' 
#' # Optimal action value function
#' matrix(apply(res$Q, 1, max), ncol = 10, byrow = TRUE)
#' 
#' # Optimal policy
#' matrix(max.col(res$Q) - 1, ncol = 10, byrow = TRUE)
sarsa <- function(envir, lambda = 0, n.steps = 100, learning.rate = 0.1, 
  epsilon = 0.1, discount.factor = 1, seed = NULL) {
  
  # input checking
  if (!is.null(seed)) set.seed(seed)
  
  time.steps.episode = c()
  episode.finished.after = rep(0, 1000)
  rewards.per.episode = rep(0, 1000)
  n.states = envir$n.states
  n.actions = envir$n.actions
  Q = matrix(0, nrow = n.states, ncol = n.actions)
  eligibility = matrix(0, nrow = n.states, ncol = n.actions)
  
  envir$reset()
  state = envir$state
  action = sample_epsilon_greedy_action(Q[state + 1, ], epsilon = epsilon)
  
  k = 0
  j = 0
  reward.sum = 0
  
  for (i in seq_len(n.steps)) {
    # if (i %% 100 == 0) {
    #   print(paste("Step:", i))
    # }
    
    envir$step(action)
    next.action = sample_epsilon_greedy_action(Q[state + 1, ], epsilon = epsilon)
    next.state = envir$state
    
    reward = envir$reward
    reward.sum = reward.sum + reward
    
    indicator = matrix(0, nrow = n.states, ncol = n.actions)
    indicator[state + 1, action + 1] = 1
    
    eligibility = discount.factor * lambda * eligibility + indicator
    TD.target = envir$reward + discount.factor * Q[next.state + 1, next.action + 1]
    TD.error = TD.target - Q[state + 1, action + 1]
    Q = Q + learning.rate * TD.error * eligibility
    
    state = next.state
    action = next.action
    
    j = j + 1
    
    if (envir$episode.over == TRUE) {
      k = k + 1
      episode.finished.after[k] = j
      rewards.per.episode[k] = reward.sum
      print(paste("Episode", k, "finished after", j, "time steps."))
      # print(paste("Average Reward:", sum(rewards.per.episode) / i))
      # if (i %% 100 == 0) {
      #   epsilon = epsilon / 2
      #   print(paste("Average Reward of last 100 episodes:", sum(rewards.per.episode[seq(i - 99, i)]) / 100))
      # }
      time.steps.episode = append(time.steps.episode, i)
      envir$reset()
      state = envir$state
      action = sample_epsilon_greedy_action(Q[state + 1, ], epsilon = epsilon)
      eligibility = matrix(0, nrow = n.states, ncol = n.actions)
      j = 0
      reward.sum = 0
    }
  }
  
  list(Q = Q, episode.finished.after = episode.finished.after, 
    rewards.per.episode = rewards.per.episode, time.steps.episode = time.steps.episode)
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
  
  action - 1
}

# Argmax (ties broken randomly)
# x numeric matrix or numeric vector
argmax <- function(x) {
  nnet::which.is.max(x)
}
