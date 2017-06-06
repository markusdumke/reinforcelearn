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
#' 
#' @inheritParams params
#' @export
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @import checkmate
#' @seealso [td]
#' @examples 
#' set.seed(26)
#' grid = gridworld$new()
#' Gridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix)
#'   
#' # Define random policy
#' random.policy = matrix(1 / Gridworld1$n.actions, nrow = Gridworld1$n.states, 
#'   ncol = Gridworld1$n.actions)
#'   
#' # Estimate state value function with Monte Carlo prediction
#' v = predictMC(Gridworld1, random.policy, n.episodes = 100, 
#'   method = "first-visit", learning.rate = NULL)
#' v = predictMC(Gridworld1, random.policy, n.episodes = 100, 
#'   method = "every-visit", learning.rate = NULL)
predictMC = function(envir, policy, n.episodes = 100L, v = NULL,  
  method = c("first-visit", "every-visit"), discount.factor = 1, 
  learning.rate = 0.1, print.out = 50L) {
  
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
    if (i %% print.out == 0) {
      print(paste("Episode:", i))
      print(v)
    }
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
  }
  v
}

sampleEpisode = function(policy, envir, initial.state = NULL, initial.action = NULL) {
  
  rewards = numeric(0)
  if (!is.null(initial.action)) {
    actions = initial.action
  } else {
    actions = integer(0)
  }
  if (!is.null(initial.state)) {
    states = initial.state
    envir$state = initial.state
  } else {
    envir$reset()
    states = envir$state
  }
  
  i = 1
  
  while (envir$episode.over == FALSE) {
    actions = append(actions, sample(envir$actions, prob = policy[states[i], ], size = 1))
    envir$step(actions[i])
    states = append(states, envir$state)
    rewards = append(rewards, envir$reward)
    i = i + 1
  }
  list(states = states, actions = c(actions, NA), rewards = c(NA, rewards))
}

# Estimate return
estimateReturn <- function(rewards, discount.factor) {
  sum(discount.factor ^ seq(0, length(rewards) - 1) * rewards)
}
