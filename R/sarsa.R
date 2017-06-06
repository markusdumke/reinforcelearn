#' SARSA (Table-lookup)
#' 
#' Sarsa is an on-policy TD control algorithm to find the optimal policy.
#' The action value function Q will be updated 
#' towards the action value function of the next state S' and next action A' 
#' using an epsilon-greedy policy derived from Q.
#' The update formula is: 
#' \deqn{Q(S, A) <- Q(S, A) + \alpha[R + \gamma Q(S', A') - Q(S, A)]}
#'  
#' @inheritParams params
#'
#' @importFrom stats runif
#' @importFrom nnet which.is.max
#' @return optimal action value function Q
#' @export
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @seealso [td]
#' @seealso [qlearning]
#' @examples 
#' grid = WindyGridworld$new()
#' WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix, 
#'   initial.state = 30L)
#' res = sarsa(WindyGridworld1, n.episodes = 100, seed = 123)
#' 
sarsa <- function(envir, lambda = 0, n.episodes = 100, learning.rate = 0.1, 
  epsilon = 0.1, epsilon.decay = 0.5, epsilon.decay.after = 100L, 
  initial.value = 0L, discount.factor = 1, seed = NULL) {
  
  # input checking
  stopifnot(envir$state.space == "Discrete")
  if (!is.null(seed)) set.seed(seed)
  
  n.states = envir$n.states
  n.actions = envir$n.actions
  Q = matrix(initial.value, nrow = n.states, ncol = n.actions)
  steps.per.episode = rep(0, n.episodes)
  rewards.per.episode = rep(0, n.episodes)
  
  for (i in seq_len(n.episodes)) {
    
    eligibility = matrix(0, nrow = n.states, ncol = n.actions)
    envir$reset()
    state = envir$state
    j = 0
    reward.sum = 0
    action = sampleAction(Q[state + 1, ], epsilon)
    
    while (envir$episode.over == FALSE) {
      
      envir$step(action)
      next.state = envir$state
      reward = envir$reward
      reward.sum = reward.sum + reward
      next.action = sampleAction(Q[next.state + 1, ], epsilon)
      
      indicator = matrix(0, nrow = n.states, ncol = n.actions)
      indicator[state + 1, action + 1] = 1
      eligibility = discount.factor * lambda * eligibility + indicator
      
      # update Q for visited state-action pair
      TD.target = reward + discount.factor * Q[next.state + 1, next.action + 1]
      TD.error = TD.target - Q[state + 1, action + 1] 
      Q = Q + learning.rate * TD.error * eligibility
      
      j = j + 1
      state = next.state
      action = next.action
      
      if (envir$episode.over) {
        steps.per.episode[i] = j
        rewards.per.episode[i] = reward.sum
        print(paste("Episode", i, "finished after", j, "time steps."))
        if (i %% epsilon.decay.after == 0) {
          epsilon = epsilon.decay * epsilon
          print(paste("Average Reward of last", epsilon.decay.after, "episodes:", sum(rewards.per.episode[seq(i - epsilon.decay.after + 1, i)]) / epsilon.decay.after))
        }
        break
      } 
    }
  }
  
  list(Q = Q, steps.per.episode = steps.per.episode, 
    rewards.per.episode = rewards.per.episode)
}
