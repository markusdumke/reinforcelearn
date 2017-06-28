#' On-policy Monte Carlo Control (Table-lookup)
#' 
#' Find optimal policy by sampling full episodes of experience and 
#' estimate values of every state-action pair. 
#' To ensure that every state-action pair is visited, an epsilon-greedy policy is used, 
#' so that every state-action pair has a positive probability of being selected.
#' 
#' Works only for episodic tasks (i.e. there must be a terminal state)! 
#' This method uses the first-visit Monte Carlo policy evaluation.
#' @return list with action value function Q and policy
#' @inheritParams params
#' @export
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @seealso [predictMonteCarlo]
#' @examples 
#' grid = makeEnvironment(transition.array = windyGridworld$transitions,
#'   reward.matrix = windyGridworld$rewards,
#'   initial.state = 30L)
#' # res = MonteCarloControl(grid, n.episodes = 100)
MonteCarloControl = function(envir, n.episodes = 100L, discount.factor = 1, 
  learning.rate = 0.1, epsilon = 0.1, initial.policy = NULL) { # decrease epsilon
  
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  checkmate::assertNumber(discount.factor, lower = 0, upper = 1)
  checkmate::assertInt(n.episodes, null.ok = TRUE)
  
  if (is.null(initial.policy)) {
    initial.policy = matrix(1 / envir$n.actions, nrow = envir$n.states, 
      ncol = envir$n.actions)
  } else {
    if (envir$n.actions != ncol(initial.policy)) {
      stop("The number of columns of the policy must be equal to the number of actions.")
    }
    if (envir$n.states != nrow(initial.policy)) {
      stop("The number of rows of the policy must be equal to the number of states.")
    }
    if (any(rowSums(initial.policy) != 1)) {
      stop("The probabilities of each row of the policy must sum to 1.")
    }
  }
  policy = initial.policy
  Q = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  
  for (i in seq_len(n.episodes)) {
    envir$reset()
    episode = sampleEpisode(policy, envir)
    unique.state.action.pairs = unique(data.frame(states = episode$states[seq_len(length(episode$states) - 1)], 
      actions = episode$actions[seq_len(length(episode$actions) - 1)]))

    for (j in seq_len(nrow(unique.state.action.pairs))) {
      first.occurence = min(which(episode$states == unique.state.action.pairs[j, 1] & 
          episode$actions == unique.state.action.pairs[j, 2]))
      sequ = seq(first.occurence + 1, length(episode$rewards))
      rewards = episode$rewards[sequ]
      G = estimateReturn(rewards, discount.factor)
      Q[as.matrix(unique.state.action.pairs[j, ] + 1)] = Q[as.matrix(unique.state.action.pairs[j, ] + 1)] + 
        learning.rate * (G - Q[as.matrix(unique.state.action.pairs[j, ] + 1)])
    }
    policy = returnPolicy(Q, epsilon)
  }
  list(Q = Q, policy = policy)
}
