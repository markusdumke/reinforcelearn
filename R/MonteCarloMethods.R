#' Monte Carlo Prediction (Table-lookup)
#' 
#' Predict state value function v with Monte Carlo methods. The state value 
#' function is estimated from mean returns of episodes.
#' 
#' Only works for episodic tasks (i.e. there must be at least one terminal 
#' state)! An incremental mean update is implemented. Use a high learning.rate to
#' give recent episodes a higher weight if you have a non-stationary environment
#' . First-visit Monte Carlo estimates the return following the first visit to 
#' a state, every-visit Monte Carlo following all visits to a state in the episode. Returns 
#' are averaged over multiple episodes. The update rule is
#' \deqn{V(S) <- V(S) + \alpha[G - V(S')]}
#' @return [\code{numeric}] \cr
#'   Returns the state value function v.
#' @inheritParams params
#' @export
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @import checkmate
#' @seealso \code{\link{td}}
#' @examples 
#' set.seed(26)
#' grid = makeEnvironment(transition.array = gridworld$transitions, 
#'   reward.matrix = gridworld$rewards)
#'   
#' # Define random policy
#' random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
#'   ncol = grid$n.actions)
#'   
#' # Estimate state value function with Monte Carlo prediction
#' v = predictMonteCarlo(grid, random.policy, n.episodes = 100, 
#'   method = "first-visit", learning.rate = NULL)
#' v = predictMonteCarlo(grid, random.policy, n.episodes = 100, 
#'   method = "every-visit", learning.rate = NULL)
predictMonteCarlo = function(envir, policy, n.episodes = 100L, v = NULL,  
  method = c("first-visit", "every-visit"), discount.factor = 1, 
  learning.rate = 0.1, print.out = 50L) {
  
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  if (is.null(v)) {
    v = rep(0, envir$n.states)
  } else {
    checkmate::assertNumeric(v, len = envir$n.states)
  }
  
  if (envir$n.actions != ncol(policy)) {
    stop("The number of columns of the policy must be equal to the number of actions.")
  }
  if (envir$n.states != nrow(policy)) {
    stop("The number of rows of the policy must be equal to the number of states.")
  }
  if (any(rowSums(policy) != 1)) {
    stop("The probabilities of each row of the policy must sum to 1.")
  }
  checkmate::assertNumber(discount.factor, lower = 0, upper = 1)
  checkmate::assertInt(n.episodes, null.ok = TRUE)
  checkmate::assertInt(print.out)
  checkmate::assertChoice(method, c("first-visit", "every-visit"))
  if (!is.null(learning.rate)) {
    checkmate::assertNumber(learning.rate, lower = 0, upper = 1)
  }
  
  # save in learning.rate_input the user inputs to reuse this later
  learning.rate_input = learning.rate
  n.states = envir$n.states
  n.visits = rep(0, n.states)
  
  for (i in seq_len(n.episodes)) {
    envir$reset()
    episode = sampleEpisode(policy, envir)
    
    if (method == "first-visit") {
      # for each state visited in this episode apply mean update
      # find first occurence if state, weighted sum of following rewards
      # incremental mean update
      for (j in unique(episode$states[seq_len(length(episode$states) - 1)])) {
        first.occurence = min(which(episode$states == j))
        sequ = seq(first.occurence + 1, length(episode$rewards))
        n.visits[j + 1] = n.visits[j + 1] + 1
        rewards = episode$rewards[sequ]
        G = estimateReturn(rewards, discount.factor)
        if (is.null(learning.rate_input)) {
          learning.rate = 1 / n.visits[j + 1]
        }
        v[j + 1] = v[j + 1] + learning.rate * (G - v[j + 1])
      }
    }
    
    if (method == "every-visit") {
      for (j in unique(episode$states[seq_len(length(episode$states) - 1)])) {
        occurences = which(episode$states == j)
        for (k in occurences) {
          sequ = seq(k + 1, length(episode$rewards))
          n.visits[j + 1] = n.visits[j + 1] + 1
          rewards = episode$rewards[sequ]
          G = estimateReturn(rewards, discount.factor)
          if (is.null(learning.rate_input)) {
            learning.rate = 1 / n.visits[j + 1]
          }
          v[j + 1] = v[j + 1] + learning.rate * (G - v[j + 1])
        }
        # sequences = sapply(occurences, function(x) seq(x, length(episode$rewards))) # use vapply!
      }
    }
    if (i %% print.out == 0) {
      print(paste("Episode:", i))
      print(v)
    }
  }
  v
}

#---------------------------------------------------------------------

#' On-policy Monte Carlo Control (Table-lookup)
#' 
#' Find optimal policy by sampling full episodes of experience and 
#' estimate values of every state-action pair. 
#' To ensure that every state-action pair is visited, an epsilon-greedy policy is used, 
#' so that every state-action pair has a positive probability of being selected.
#' 
#' Works only for episodic tasks (i.e. there must be a terminal state)! 
#' This method uses the first-visit Monte Carlo policy evaluation.
#' @return [\code{list(2)}] \cr
#'   Returns the action value function Q and policy
#' @inheritParams params
#' @export
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @seealso \code{\link{predictMonteCarlo}}
#' @examples 
#' grid = makeEnvironment(transition.array = windyGridworld$transitions,
#'   reward.matrix = windyGridworld$rewards,
#'   initial.state = 30L)
#' # res = MonteCarloControl(grid, n.episodes = 100)
MonteCarloControl = function(envir, n.episodes = 100L, discount.factor = 1, 
  learning.rate = 0.1, epsilon = 0.1, initial.policy = NULL) { # decrease epsilon
  
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  checkmate::assertNumber(discount.factor, lower = 0, upper = 1)
  checkmate::assertInt(n.episodes, null.ok = TRUE)
  checkmate::assertNumber(epsilon, lower = 0, upper = 1)
  checkmate::assertNumber(learning.rate, lower = 0, upper = 1)
   
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
