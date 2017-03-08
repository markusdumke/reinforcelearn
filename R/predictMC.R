#' Monte Carlo Prediction
#' 
#' Predict state value function v with Monte Carlo methods. The state value 
#' function is estimated from mean returns of episodes.
#' 
#' Only works for episodic tasks (i.e. there must be at least one terminal 
#' state)! An incremental mean update is implemented. Use a high alpha value to
#' give recent episodes a higher weight if you have a non-stationary environment
#' . First-visit Monte Carlo estimates the return following the first visit to 
#' a state, every-visit Monte Carlo following all visits in the episode. Returns 
#' are averaged over multiple episodes.
#' 
#' @inheritParams evaluatePolicy
#' @param n.episodes scalar integer: the number of episodes
#' @param method character: first-visit or every-visit method
#' @param alpha scalar numeric between 0 and 1: weighting factor in the 
#'   incremental mean update.Useful in non-stationary environments, giving high 
#'   value to the last observed returns. If NULL the exact mean with alpha = 1 /
#'   number of episodes will be used.
#' @export
#' @import checkmate
#' @seealso [TD]
#' @examples 
#' set.seed(1477)
#' grid = gridworld$new()
#' 
#' # Define random policy
#' random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
#'   ncol = grid$n.actions)
#'   
#' # Estimate state value function with Monte Carlo prediction
#' v = predictMC(random.policy, grid, n.episodes = 100, method = "first-visit")
#' v = predictMC(random.policy, grid, n.episodes = 100, method = "every-visit")
predictMC = function(policy, envir, n.episodes = 10, discount.factor = 1, 
  method = c("first-visit, every-visit"), alpha = NULL) {
  
  # save in alpha_input the user inputs to reuse this later
  alpha_input = alpha
  check_choice(method, c("first-visit, every-visit"))
  if (!is.null(alpha)) {
    check_number(alpha, lower = 0, upper = 1)
  }
  check_number(discount.factor, lower = 0, upper = 1)
  
  n.states = envir$n.states
  v = rep(0, n.states)
  n.visits = rep(0, n.states)
  possible.states = envir$states[envir$states != envir$terminal.states]
  
  # to do: parallelize episodes
  for (i in seq_len(n.episodes)) {
    if (i %% 50 == 0) {
      print(paste("Episode:", i))
    }
    envir$setEpisodeOverFalse()
    initial.state = sample(possible.states, 1)
    episode = sampleEpisode(policy, envir, initial.state)
    
    if (method == "first-visit") {
      # for each state visited in this episode apply mean update
      # find first occurence if state, weighted sum of following rewards
      # incremental mean update
      for (j in unique(episode$states)) { # what if j character?
        first.occurence = min(which(episode$states == j))
        sequ = seq(first.occurence + 1, length(episode$rewards))
        n.visits[j] = n.visits[j] + 1
        rewards = episode$rewards[sequ]
        G = estimateReturn(rewards, discount.factor)
        if (is.null(alpha_input)) {
          alpha = 1 / n.visits[j]
        }
        v[j] = v[j] + alpha * (G - v[j])
      }
    }
    
    if (method == "every-visit") {
      for (j in unique(episode$states)) {
        occurences = which(episode$states == j)
        for (k in occurences) {
          sequ = seq(k, length(episode$rewards))
          n.visits[j] = n.visits[j] + 1
          rewards = episode$rewards[sequ]
          G = estimateReturn(rewards, discount.factor)
          if (is.null(alpha_input)) {
            alpha = 1 / n.visits[j]
          }
          v[j] = v[j] + alpha * (G - v[j])
        }
        # sequences = sapply(occurences, function(x) seq(x, length(episode$rewards))) # use vapply!
      }
    }
  }
  return(v)
}


#' Sample episode
#' 
#' Sample an episode in an environment given a policy. 
#' Note that this only works for episodic environments 
#' (e.g. there must be at least one terminal state). There is no action in the 
#' last time step and no reward for the first time step: 
#'  \tabular{rrrrr}{
#'  S_1 \tab S_2 \tab ... \tab S_T-1 \tab S_T \cr
#'  A_1 \tab A_2 \tab ... \tab A_T-1 \tab NA \cr
#'  NA \tab R_2 \tab ... \tab R_T-1 \tab R_T \cr
#' }
#' S1, S2, ..., ST-1, ST
#' A1, A2, ..., AT-1, NA
#' NA, R2, ..., RT-1, RT
#'
#' @inheritParams evaluatePolicy
#' @param initial.state integer: the initial state
#' @param initial.action character: the initial action. Default is NULL, 
#' then first action will be sampled from policy.
#'
#' @return a list with sampled actions, states and returns of the episode.
#' @export
#' @examples
#' set.seed(26)
#' grid = gridworld$new(shape = c(4, 4), terminal.states = c(1, 16))
#' 
#' # Define random policy
#' random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
#'   ncol = grid$n.actions)
#' 
#' # Sample an episode for the random.policy
#' episode = sampleEpisode(random.policy, grid, initial.state = 3)
#' print(episode$actions)
#' print(episode$rewards)
#' print(episode$states)
#' 
#' # Specify an initial action
#' grid$setEpisodeOverFalse()
#' episode = sampleEpisode(random.policy, grid, initial.state = 7, 
#'   initial.action = "right")
#' print(episode$actions)
#' print(episode$rewards)
#' print(episode$states)
#' 
sampleEpisode = function(policy, envir, initial.state, initial.action = NULL) {
  
  states = initial.state
  rewards = numeric(0)
  if (!is.null(initial.action)) {
    sampled.actions = initial.action
  } else {
    sampled.actions = character(0)
  }
  
  i = 1
  
  while (envir$episode.over == FALSE) {
    sampled.actions = append(sampled.actions, 
      sample(envir$actions, prob = policy[states[i], ], size = 1))
    envir$step(states[i], sampled.actions[i])
    states = append(states, envir$next.state)
    rewards = append(rewards, envir$reward)
    i = i + 1
  }
  
  return(list(states = states, actions = c(sampled.actions, NA), rewards = c(NA, rewards)))
}

# Estimate return
estimateReturn <- function(rewards, discount.factor) {
  sum(discount.factor ^ seq(0, length(rewards) - 1) * rewards)
}
