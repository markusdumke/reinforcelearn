#' Monte Carlo Control with Exploring Starts
#'
#' @inheritParams evaluatePolicy
#'
#' @return the optimal policy and optimal action value function q
#' @export
#' @examples 
#' set.seed(123)
#' grid = gridworld$new()
#'   
#' # Estimate optimal action value function with Monte Carlo Exploring Starts
#' Q = MonteCarloES(grid)
MonteCarloES <- function(envir, discount.factor = 1) {
  
  # input checking
  
  # initialize action value function Q(s, a) and policy pi randomly
  n.states = envir$n.states
  n.actions = envir$n.actions
  Q = matrix(0, nrow = n.states, ncol = n.actions)
  policy = matrix(1 / n.actions, nrow = n.states, ncol = n.actions)
  n.visits = matrix(0, nrow = n.states, ncol = n.actions)
  # returns = matrix(1 / n.actions, nrow = n.states, ncol = n.actions) # array?
  
  possible.states = envir$states[envir$states != envir$terminal.states]
  
  # while (epsilon) ...
  for (i in 1:100) {
    # sample initial state and initial action (all pairs have probability > 0)
    state = sample(possible.states, size = 1)
    # to do: sample only actions that are possible from the state!
    action = sample(envir$actions, size = 1)
    
    envir$setEpisodeOverFalse()
    episode = sampleEpisode(policy, envir, initial.state = state, 
      initial.action = action)
    
    # for each state-action pair:
    for (s in unique(episode$states)) {
      for (a in unique(episode$actions[episode$states == s])) {
        first.occurence = min(which(episode$states == s & episode$actions == a))
        sequ = seq(first.occurence, length(episode$rewards))
        n.visits[s, a] = n.visits[s, a] + 1
        rewards = episode$rewards[sequ]
        
        # return following first occurence
        G = estimateReturn(rewards, discount.factor) # append G to returns
        Q(s, a) = Q(s, a) + 1 / n.visits[s, a] * G
      }
    }
    
    # implement greedy policy update
    # for (s in unique(episode$states)) {
    #   policy <- argmax(Q(s, a)) # vectorize!
    # }
  }
  
  Q
  # list(optimal.policy, optimal.value.function)
}

