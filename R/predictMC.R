#' Monte Carlo Prediction
#' 
#' Predict state value function with Monte Carlo methods
#' first.visit MC, evaluate given policy
#' 
#' @inheritParams evaluatePolicy
#' @param n.episodes integer: the number of episodes
#' @param method chacracter: first-visit or every-visit method
#' @export
#' @examples 
#' set.seed(1477)
#' grid = gridworld_R6$new()
#' 
#' # Define random policy
#' n.states = nrow(grid$reward.matrix)
#' n.actions = ncol(grid$reward.matrix)
#' random.policy = matrix(1 / n.actions, nrow = n.states, ncol = n.actions)
#' 
#' # Estimate state value function with Monte Carlo first visit prediction
#' v = predictMC(random.policy, grid, n.episodes = 10000, method = "first-visit")
#' 
#' # Compare results with expected result
#' v.expected = c(0, -14, -20, -22, -14, -18, -20, -20,
#'                -20, -20, -18, -14, -22, -20, -14, 0)
#' all.equal(v, v.expected, tolerance = 0.5)
#' 
predictMC = function(policy, envir, n.episodes = 10, discount.factor = 1, 
  method = c("first-visit, every-visit")) {
  
  # to implement: discount.factor currently not in use!
  n.states = nrow(policy)
  
  # initialize v to zeros
  v = rep(0, n.states)
  
  # return G following the first occurence of state s
  G = rep(0, n.states) 
  n.visits = rep(0, n.states)
  
  possible.states = envir$states[envir$states != envir$terminal.states]
  
  if (method == "first-visit") {
    for (i in seq_len(n.episodes)) {
      if (i %% 10 == 0) {
        print(paste("Episode:", i))
      }
      envir$setEpisodeOverFalse()
      initial.state = sample(possible.states, 1)
      episode = sampleEpisode(policy, envir, initial.state)
      
      # For each state visited in this episode apply mean update
      for (j in unique(episode$states)) {
        first.occurence = min(which(episode$states == j))
        sequence = seq(first.occurence, length(episode$rewards))
        G[j] = G[j] + sum(episode$rewards[sequence]) # insert discount.factor -> weighted average
        n.visits[j] = n.visits[j] + 1
      }
      v = G / n.visits
    }
  }
  
  v[envir$terminal.states] = 0
  return(v)
}


#' Sample episode
#' 
#' Sample an episode in an environment given a policy. 
#' Note that this only works for episodic environments 
#' (e.g. there must be a terminal state).
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
  
  while(envir$episode.over == FALSE) {
    sampled.actions = append(sampled.actions, sample(envir$actions, size = 1))
    envir$step(states[i], sampled.actions[i])
    states = append(states, envir$next.state)
    rewards = append(rewards, envir$reward)
    i = i + 1
  }
  
  return(list(states = states, actions = sampled.actions, rewards = rewards))
}
