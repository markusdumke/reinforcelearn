#' Q(sigma) (Table-lookup)
#' 
#' Q(sigma) is a generalization of Sarsa and Expected Sarsa algorithms.
#'  
#' @inheritParams params
#'
#' @return optimal action value function Q
#' @export
#' @references De Asis et al. (2017): Multi-step Reinforcement Learning: A Unifying Algorithm
#' @seealso [sarsa]
#' @seealso [qlearning]
#' @examples 
#' grid = makeWindyGridworld()
#' WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix, initial.state = 30L)
#' res = qSigma(WindyGridworld1, n.episodes = 100, seed = 123)
#' 
qSigma <- function(envir, sigma = 1, lambda = 0, n.episodes = 100, learning.rate = 0.1, 
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
    policy = 
    
    while (envir$episode.over == FALSE) {
      envir$step(action)
      next.state = envir$state
      reward = envir$reward
      reward.sum = reward.sum + reward
      
      if (envir$episode.over == TRUE) {
        td.error = reward - Q[state + 1, action + 1]
      } else {
        next.action = sampleAction(Q[next.state + 1, ], epsilon)
        policy = returnPolicy(Q[next.state + 1, , drop = FALSE], epsilon)
        V = sum(policy * Q[next.state + 1, ])
        td.error = reward + discount.factor * (sigma * Q[next.state + 1, action + 1] + 
            (1 - sigma) * V) - Q[state + 1, action + 1]
      }
      
      indicator = matrix(0, nrow = n.states, ncol = n.actions)
      indicator[state + 1, action + 1] = 1
      eligibility = discount.factor * lambda * eligibility + indicator
      
      # update Q for visited state-action pair
      Q = Q + learning.rate * td.error * eligibility
      
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
