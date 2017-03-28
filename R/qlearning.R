#' Q-Learning (Table-lookup)
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
#' @param render logical scalar: should the environment be rendered
#'
#' @return optimal action value function
#' @seealso [dqlearning]
#' @references [Sutton and Barto (2017) page 140](https://webdocs.cs.ualberta.ca/~sutton/book/bookdraft2016sep.pdf#page=158)
#' @export
#' @examples
#' # grid = gridworld$new()
#' # Q = qlearning(grid, n.episodes = 1000)
#' 
#' \dontrun{
#' # Make sure you have gym-http-api and python installed.
#' # Then start a server from command line by running: python gym_http_server.py
#' FrozenLake = makeEnvironment("FrozenLake-v0")
#' Q = qlearning(FrozenLake, n.episodes = 10)
#' }
qlearning <- function(envir, n.episodes = 10, learning.rate = 0.1, epsilon = 0.1, 
  discount.factor = 1, seed = NULL, render = TRUE) {
  
  # input checking
  stopifnot(envir$state.space == "Discrete")
  if (!is.null(seed)) set.seed(seed)

  n.states = envir$n.states
  n.actions = envir$n.actions
  Q = matrix(0, nrow = n.states, ncol = n.actions)
  episode.finished.after = rep(0, n.episodes)
  rewards.per.episode = rep(0, n.episodes)
  
  for (i in seq_len(n.episodes)) {
    
    envir$reset()
    state = envir$state
    j = 0
    reward.sum = 0
    
    while (envir$episode.over == FALSE) {
      
      action = sample_epsilon_greedy_action(Q[state + 1, ], epsilon) - 1
      envir$step(action, render = render)
      next.state = envir$state
      reward = envir$reward
      reward.sum = reward.sum + reward
      
      # update Q for visited state-action pair maximizing over Q values of next state
      TD.target = reward + discount.factor * max(Q[next.state + 1, ]) 
      TD.error = TD.target - Q[state + 1, action + 1] 
      Q[state + 1, action + 1] = Q[state + 1, action + 1] + learning.rate * TD.error
      state = next.state
      
      j = j + 1
      if (envir$episode.over) {
        episode.finished.after[i] = j
        rewards.per.episode[i] = reward.sum
        # print(paste("Episode", i, "finished after", j, "time steps."))
        # print(paste("Average Reward:", sum(rewards.per.episode) / i))
        if (i %% 100 == 0) {
          epsilon = epsilon / 2
          print(paste("Average Reward of last 100 episodes:", sum(rewards.per.episode[seq(i - 99, i)]) / 100))
        }
        break
      } 
    }
  }
  
  list(Q = Q, episode.finished.after = episode.finished.after, 
    rewards.per.episode = rewards.per.episode)
}
