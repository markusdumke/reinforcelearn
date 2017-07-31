#' Solve multi-armed bandit
#' 
#' Multi-armed bandits are a simplified reinforcement learning problem, 
#' each arm of the bandit pays off a reward and the goal is to maximize 
#' this reward, i.e. to choose the best arm. The arms of the bandit 
#' can be seen as actions, after each action the episode ends (there 
#' are no states). To find the best action, the algorithm is faced with 
#' a tradeoff between exploration and exploitation.
#' 
#' 
#' @inheritParams documentParams
#' @param step [\code{function}] \cr 
#'   A function, which takes an action (a scalar 
#'   integer) as first argument and returns a numeric scalar reward.
#' @param n.actions [\code{integer(1)}] \cr 
#'   Number of actions.
#' @param alpha [\code{numeric(1)}] \cr 
#'   Parameter of gradient bandit algorithm, higher alpha 
#'   value gives more weight to recent rewards 
#'   (useful for non-stationary environments)
#' 
#' @return [\code{numeric}] \cr
#' Returns the action values for the arms of the bandit, 
#' for gradient-bandit action selection the probabilities for 
#' each action will be returned.
#' @export
#'
#' @examples
#' set.seed(123)
#' # Define reward function
#' step = function(action) {
#'   if (action == 0) {
#'     reward = rnorm(1, mean = 1, sd = 1)
#'   }
#'   if (action == 1) {
#'     reward = rnorm(1, mean = 2, sd = 4)
#'   }
#'   if (action == 2) {
#'     reward = runif(1, min = 0, max = 5)
#'   }
#'   if (action == 3) {
#'     reward = rexp(1, rate = 0.25)
#'   }
#'   reward
#' }
#' solveBandit(step, n.actions = 4, n.episodes = 1000, 
#'   action.selection = "greedy")
#' solveBandit(step, n.actions = 4, n.episodes = 1000, 
#'   action.selection = "epsilon-greedy", epsilon = 0.5)
#' solveBandit(step, n.actions = 4, n.episodes = 1000, 
#'   action.selection = "greedy", 
#'   initial.value = 5, initial.visits = 100)
#' solveBandit(step, n.actions = 4, n.episodes = 1000, 
#'   action.selection = "UCB", C = 2)
#' # true values: 1, 2, 2.5, 4
#' # Gradient bandit algorithm
#' solveBandit(step, n.actions = 4, n.episodes = 10000, 
#'   action.selection = "gradient-bandit", alpha = 0.1)
solveBandit = function(step, n.actions, n.episodes = 100L,
  action.selection = c("epsilon-greedy", "greedy", "UCB", "gradient-bandit"), epsilon = 0.1, 
  epsilon.decay = 0.5, epsilon.decay.after = 100L, alpha = 0.1,
  initial.value = 0, initial.visits = 0L, C = 2) {
  
  checkmate::assertInt(n.episodes, lower = 1)
  checkmate::assertChoice(action.selection, c("epsilon-greedy", 
    "greedy", "UCB", "gradient-bandit"))
  checkmate::assertNumber(epsilon, lower = 0, upper = 1)
  checkmate::assertNumber(epsilon.decay, lower = 0, upper = 1)
  checkmate::assertInt(epsilon.decay.after, lower = 1)
  checkmate::assertNumber(alpha, lower = 0, upper = 1)
  checkmate::assertNumber(initial.value)
  checkmate::assertInt(initial.visits, lower = 0)
  checkmate::assertNumber(C)
  
  action.selection = match.arg(action.selection)
  action.visits = rep(initial.visits, n.actions)
  rewards = rep(initial.value * initial.visits, n.actions)
  Q = rep(initial.value, n.actions)
  H = rep(0, n.actions)
  
  for (i in seq_len(n.episodes)) {
    if (action.selection == "greedy") {
      action = argmax(Q) - 1L
    }
    if (action.selection == "epsilon-greedy") {
      if (i %% epsilon.decay.after == 0) {
        epsilon = epsilon * epsilon.decay
      }
      action = sampleAction(Q, epsilon)
    }
    if (action.selection == "UCB") {
      if (any(action.visits == 0)) {
        action = sample(which(action.visits == 0), 1) - 1L
      } else {
        action = argmax(Q + sqrt(C * log(i) / action.visits)) - 1L 
      }
    }
    if (action.selection == "gradient-bandit") {
      Q = exp(H) / sum(exp(H))
      action = sample(seq_len(n.actions) - 1, 1, prob = Q)
    }
    reward = step(action)
    
    action.visits[action + 1] = action.visits[action + 1] + 1L
    rewards[action + 1] = rewards[action + 1] + reward
    if (action.selection != "gradient-bandit") {
      Q[action + 1] = rewards[action + 1] / action.visits[action + 1]
    }
    
    if (action.selection == "gradient-bandit") {
      total.reward = sum(rewards)
      total.action.visits = sum(action.visits)
      average.reward = total.reward / total.action.visits
      H[ - action + 1] = H[ - action + 1] - alpha * (reward - average.reward) * Q[ - action + 1]
      H[action + 1] = H[action + 1] + alpha * 
        (reward - average.reward) * (1 - Q[action + 1])
    }
  }
  Q
}

# alpha parameter for nonstationary environments

# continuous action space
# contextual bandits

# Exploration - Exploitation
# Thompson Sampling
# Information state space
# Bayesian Bandit
# Probability matching
