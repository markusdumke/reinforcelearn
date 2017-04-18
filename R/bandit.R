#' Example Bandit
#' @export
#' @return R6 class
bandit = R6::R6Class("bandit", 
  public = list(
    actions = NULL,
    n.actions = NULL,
    action = NULL,
    reward = NULL,
    
    initialize = function() {
      self$actions = 0:3
      self$n.actions = length(self$actions)
    },
    
    step = function(action) {
      self$action = action
      if (action == 0) {
        self$reward = rnorm(1, mean = 1, sd = 1)
      }
      if (action == 1) {
        self$reward = rnorm(1, mean = 2, sd = 4)
      }
      if (action == 2) {
        self$reward = runif(1, min = 0, max = 5)
      }
      if (action == 3) {
        self$reward = rexp(1, rate = 0.25)
      }
    }
  )
)

#' Solve multi-armed bandit
#' 
#' Multi-armed bandits are a simplified reinforcement learning problem, 
#' each arm of the bandit pays off a reward and the goal is to maximize 
#' this reward, i.e. to choose the best arm. The arms of the bandit 
#' can be seen as actions, after each action the episode ends (there 
#' are no states). To find the best action, the algorithm is faced with 
#' a tradeoff between exploration and exploitation.
#' 
#' Upper-confidence-bound action selection selects actions with 
#' \deqn{argmax_a Q(a) + sqrt( (C * log(t)) / N_t(a) ),} where N_t(a) is the 
#' number of times action a was selected.
#' 
#' @inheritParams params
#' 
#' @return numeric vector: the action values for the arms of the bandit
#' @export
#'
#' @examples
#' ExampleBandit = bandit$new()
#' solveBandit(ExampleBandit, n.episodes = 1000, 
#'   action.selection = "greedy")
#' solveBandit(ExampleBandit, n.episodes = 1000, 
#'   action.selection = "epsilon-greedy", epsilon = 0.5)
#' solveBandit(ExampleBandit, n.episodes = 1000, 
#'   action.selection = "greedy", 
#'   initial.value = 5, initial.visits = 100)
#' solveBandit(ExampleBandit, n.episodes = 1000, 
#'   action.selection = "UCB", C = 2)
#' # true values: 1, 2, 2.5, 4
solveBandit = function(bandit, n.episodes = 10L,
  action.selection = c("epsilon-greedy", "greedy", "UCB"), epsilon = 0.1, 
  epsilon.decay = 0.5, epsilon.decay.after = 100L, 
  initial.value = 0, initial.visits = 0L, C = 2) {
  
  action.selection <- match.arg(action.selection)
  action.visits = rep(initial.visits, bandit$n.actions)
  rewards = rep(initial.value * initial.visits, bandit$n.actions)
  Q = rep(initial.value, bandit$n.actions)
  
  for (i in seq_len(n.episodes)) {
    if (action.selection == "greedy") {
      action = argmax(Q) - 1L
    }
    if (action.selection == "epsilon-greedy") {
      if (i %% epsilon.decay.after == 0) {
        epsilon = epsilon * epsilon.decay
      }
      action = sample_epsilon_greedy_action(Q, epsilon)
    }
    if (action.selection == "UCB") {
      if (any(action.visits == 0)) {
        action = sample(which(action.visits == 0), 1) - 1L
      } else {
        action = argmax(Q + sqrt(C * log(i) / action.visits)) - 1L 
      }
    }
    bandit$step(action)
    action.visits[action + 1] = action.visits[action + 1] + 1
    rewards[action + 1] = rewards[action + 1] + bandit$reward
    Q[action + 1] = rewards[action + 1] / action.visits[action + 1]
  }
  Q
}

# continuous action space
# contextual bandits

# Exploration - Exploitation

# greedy ok
# epsilon-greedy ok
# UCB ok
# optimistic initialization ok
# softmax
# Thompson Sampling

# Information state space
# Bayesian Bandit
# Probability matching
