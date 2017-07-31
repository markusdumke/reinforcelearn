#' SARSA (Table-lookup)
#' 
#' Sarsa is an on-policy TD control algorithm to find the optimal policy.
#' The action value function Q will be updated 
#' towards the action value function of the next state S' and next action A' 
#' using an epsilon-greedy policy derived from Q.
#' The update formula is: 
#' \deqn{Q(S, A) <- Q(S, A) + \alpha[R + \gamma Q(S', A') - Q(S, A)]}
#'  
#' @inheritParams documentParams
#'
#' @importFrom stats runif
#' @importFrom nnet which.is.max
#' @return [\code{list(3)}] \cr
#'   Returns the optimal action value function [\code{matrix}] and the 
#'   number of steps and rewards per episode [\code{numeric}]
#' @export
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @seealso \code{\link{qSigma}}
#' @seealso \code{\link{qlearning}}
#' @examples 
#' grid = makeEnvironment(transition.array = windyGridworld$transitions,
#'   reward.matrix = windyGridworld$rewards,
#'   initial.state = 30L)
#' res = sarsa(grid, n.episodes = 100, seed = 123)
#' 
sarsa = function(envir, lambda = 0, n.episodes = 100L, learning.rate = 0.1, 
  epsilon = 0.1, epsilon.decay = 0.5, epsilon.decay.after = 100L, 
  initial.value = 0, discount.factor = 1, seed = NULL) {
  
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  checkmate::assertNumber(lambda, lower = 0, upper = 1)
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
  episode.steps = rep(0, n.episodes)
  episode.rewards = rep(0, n.episodes)
  
  for (i in seq_len(n.episodes)) {
    eligibility = matrix(0, nrow = n.states, ncol = n.actions)
    envir$reset()
    state = envir$state
    j = 0
    reward.sum = 0
    action = sampleAction(Q[state + 1, ], epsilon)
    
    while (envir$done == FALSE) {
      envir$step(action)
      next.state = envir$state
      reward.sum = reward.sum + envir$reward
      next.action = sampleAction(Q[next.state + 1, ], epsilon)
      indicator = matrix(0, nrow = n.states, ncol = n.actions)
      indicator[state + 1, action + 1] = 1
      eligibility = discount.factor * lambda * eligibility + indicator
      td.target = envir$reward + discount.factor * Q[next.state + 1, next.action + 1]
      td.error = td.target - Q[state + 1, action + 1] 
      Q = Q + learning.rate * td.error * eligibility
      
      j = j + 1
      state = next.state
      action = next.action
      
      if (envir$done) {
        episode.steps[i] = j
        episode.rewards[i] = reward.sum
        print(paste("Episode", i, "finished after", j, "time steps."))
        if (i %% epsilon.decay.after == 0) {
          epsilon = epsilon.decay * epsilon
          print(paste("Average Reward of last", epsilon.decay.after, "episodes:", 
            sum(episode.rewards[seq(i - epsilon.decay.after + 1, i)]) / epsilon.decay.after))
        }
        break
      } 
    }
  }
  
  list(Q = Q, episode.steps = episode.steps, 
    episode.rewards = episode.rewards)
}
