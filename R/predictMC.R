#' Monte Carlo Prediction
#' 
#' Predict state value function v with Monte Carlo methods. The state value 
#' function is estimated from mean returns of episodes.
#' 
#' Only works for episodic tasks (i.e. there must be at least one terminal 
#' state)! An incremental mean update is implemented. Use a high alpha value to
#' give recent episodes a higher weight if you have a non-stationary environment
#' . First-visit and every-visit Mone Carlo policy evaluation are implemented.
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
#' @examples 
#' set.seed(1477)
#' grid = gridworld_R6$new()
#' 
#' # Define random policy
#' n.states = nrow(grid$reward.matrix)
#' n.actions = ncol(grid$reward.matrix)
#' random.policy = matrix(1 / n.actions, nrow = n.states, ncol = n.actions)
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
  
  n.states = nrow(policy)
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
      for (j in unique(episode$states)) {
        first.occurence = min(which(episode$states == j))
        sequence = seq(first.occurence, length(episode$rewards))
        n.visits[j] = n.visits[j] + 1
        G = sum(discount.factor ^ sequence * episode$rewards[sequence])
        alpha = 1 / n.visits[j]
        v[j] = v[j] + alpha * (G - v[j])
      }
    }
    
    if (method == "every-visit") {
      for (j in unique(episode$states)) {
        occurences = which(episode$states == j)
        for (k in occurences) {
          sequence = seq(k, length(episode$rewards))
          n.visits[j] = n.visits[j] + 1
          G = sum(discount.factor ^ sequence * episode$rewards[sequence])
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
#' (e.g. there must be at least one terminal state).
#'
#' @inheritParams evaluatePolicy
#' @param initial.state integer: the initial state
#'
#' @return a list with sampled actions, states and returns of the episode.
#' @export
#'
#' @examples
#' set.seed(27)
#' grid = gridworld_R6$new(shape = c(4, 4), terminal.states = c(1, 16))
#'
#' initial.state = 3
#' 
#' # make random policy
#' n.states = nrow(grid$reward.matrix)
#' n.actions = ncol(grid$reward.matrix)
#' random.policy = matrix(1 / n.actions, nrow = n.states, ncol = n.actions)
#' 
#' # sample an episode for the random.policy
#' episode = sampleEpisode(random.policy, grid, initial.state)
#' print(episode$actions)
#' print(episode$rewards)
#' print(episode$states)
#' 
sampleEpisode = function(policy, envir, initial.state) {
  
  states = initial.state
  rewards = numeric(0)
  sampled.actions = character(0)
  i = 1
  
  while (envir$episode.over == FALSE) {
    sampled.actions = append(sampled.actions, 
      sample(envir$actions, prob = policy[states[i], ], size = 1))
    envir$step(states[i], sampled.actions[i])
    states = append(states, envir$next.state)
    rewards = append(rewards, envir$reward)
    i = i + 1
  }
  
  return(list(states = states, actions = sampled.actions, rewards = rewards))
}
