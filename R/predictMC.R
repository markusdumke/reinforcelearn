#' Monte Carlo Prediction (Table-lookup)
#' 
#' Predict state value function v with Monte Carlo methods. The state value 
#' function is estimated from mean returns of episodes.
#' 
#' Only works for episodic tasks (i.e. there must be at least one terminal 
#' state)! An incremental mean update is implemented. Use a high learning.rate value to
#' give recent episodes a higher weight if you have a non-stationary environment
#' . First-visit Monte Carlo estimates the return following the first visit to 
#' a state, every-visit Monte Carlo following all visits in the episode. Returns 
#' are averaged over multiple episodes. The update rule is
#' \deqn{V(S) <- V(S) + \alpha[G - V(S')]}
#' 
#' @inheritParams evaluatePolicy
#' @param n.episodes scalar integer: the number of episodes
#' @param method scalar character: first-visit or every-visit method
#' @param learning.rate scalar numeric between 0 and 1: learning rate
#' @export
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @import checkmate
#' @seealso [td]
#' @examples 
#' set.seed(26)
#' grid = gridworld$new()
#' Gridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix, terminal.states = grid$terminal.states,
#'   initial.state = grid$initial.state)
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
predictMC = function(envir, policy, n.episodes = 100, discount.factor = 1, 
  method = c("first-visit, every-visit"), learning.rate = 0.1) {
  
  # print("Currently not implemented.")
  # save in learning.rate_input the user inputs to reuse this later
  learning.rate_input = learning.rate
  check_choice(method, c("first-visit, every-visit"))
  if (!is.null(learning.rate)) {
    check_number(learning.rate, lower = 0, upper = 1)
  }
  check_number(discount.factor, lower = 0, upper = 1)

  n.states = envir$n.states
  v = rep(0, n.states)
  n.visits = rep(0, n.states)

  for (i in seq_len(n.episodes)) {
    if (i %% 50 == 0) {
      print(paste("Episode:", i))
    }
    envir$reset()
    episode = sampleEpisode(policy, envir)

    if (method == "first-visit") {
      # for each state visited in this episode apply mean update
      # find first occurence if state, weighted sum of following rewards
      # incremental mean update
      for (j in unique(episode$states)) {
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
      for (j in unique(episode$states)) {
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
#' @param initial.state integer: the initial state. Default is NULL, then the 
#' initial state will be sampled from the initial.states of the environment
#' @param initial.action integer: the initial action. Default is NULL, 
#' then first action will also be sampled from policy.
#'
#' @return a list with sampled actions, states and returns of the episode.
#' @export
#' @examples
#' set.seed(26)
#' grid = gridworld$new()
#' Gridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix, terminal.states = grid$terminal.states,
#'   initial.state = grid$initial.state)
#'   
#' # Define random policy
#' random.policy = matrix(1 / Gridworld1$n.actions, nrow = Gridworld1$n.states, 
#'   ncol = Gridworld1$n.actions)
#' 
#' # Sample an episode using the random.policy
#' episode = sampleEpisode(random.policy, Gridworld1, initial.state = 3)
#' print(episode$actions)
#' print(episode$rewards)
#' print(episode$states)
#' 
#' 
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
  states = append(states, envir$state)
  list(states = states, actions = c(actions, NA), rewards = c(NA, rewards))
}

# Estimate return
estimateReturn <- function(rewards, discount.factor) {
  sum(discount.factor ^ seq(0, length(rewards) - 1) * rewards)
}
