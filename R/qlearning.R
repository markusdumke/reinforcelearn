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
#' @inheritParams params
#'
#' @return [\code{list(3)}] \cr
#'   Returns the optimal action value function [\code{matrix}] and the 
#'   number of steps and rewards per episode [\code{numeric}]
#' @seealso \code{\link{sarsa}}
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @export
#' @examples
#' # Solve the WindyGridworld environment using Q-Learning
#' grid = makeEnvironment(transition.array = windyGridworld$transitions,
#'   reward.matrix = windyGridworld$rewards,
#'   initial.state = 30L)
#' res = qlearning(grid, n.episodes = 100, seed = 123)
#' 
qlearning <- function(envir, n.episodes = 100L, learning.rate = 0.1, 
  epsilon = 0.1, epsilon.decay = 0.5, epsilon.decay.after = 100L, 
  initial.value = 0, discount.factor = 1, seed = NULL) {
  
  # input checking
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  checkmate::assertNumber(discount.factor, lower = 0, upper = 1)
  checkmate::assertInt(n.episodes, lower = 1)
  checkmate::assertNumber(learning.rate, lower = 0, upper = 1)
  checkmate::assertNumber(epsilon, lower = 0, upper = 1)
  checkmate::assertNumber(epsilon.decay, lower = 0, upper = 1)
  checkmate::assertInt(epsilon.decay.after, lower = 1)
  checkmate::assertNumber(initial.value)
  checkmate::assertInt(seed, lower = 1, null.ok = TRUE)
  if (!is.null(seed)) set.seed(seed)
  
  n.states = envir$n.states
  n.actions = envir$n.actions
  Q = matrix(initial.value, nrow = n.states, ncol = n.actions)
  steps.per.episode = rep(0, n.episodes)
  rewards.per.episode = rep(0, n.episodes)
  
  for (i in seq_len(n.episodes)) {
    
    envir$reset()
    state = envir$state
    j = 0
    reward.sum = 0
    
    while (envir$episode.over == FALSE) {
      
      action = sampleAction(Q[state + 1, ], epsilon)
      envir$step(action)
      next.state = envir$state
      reward = envir$reward
      reward.sum = reward.sum + reward
      
      # update Q for visited state-action pair maximizing over Q values of next state
      TD.target = reward + discount.factor * max(Q[next.state + 1, ]) 
      TD.error = TD.target - Q[state + 1, action + 1] 
      Q[state + 1, action + 1] = Q[state + 1, action + 1] + learning.rate * TD.error
      print(Q)
      state = next.state
      j = j + 1
      
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
