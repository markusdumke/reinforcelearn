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
      # self$reward = self$sampleReward(action)
    }#,
    
    # sampleReward = function(action) {
    #   if (action == 0) {
    #     rnorm(1, mean = 1, sd = 1)
    #   }
    #   if (action == 1) {
    #     rnorm(1, mean = 2, sd = 4)
    #   }
    #   if (action == 2) {
    #     ruinf(1, min = 0, max = 5)
    #   }
    #   if (action == 3) {
    #     rexp(1, rate = 0.25)
    #   }
    # }
  )
)

#' Solve multi-armed bandit
#'
#' @param bandit R6 class bandit
#' @param n.episodes scalar integer: number of episodes
#' @param action.selection scalar character: which method to use for 
#' action selection, e.g. "epsilon-greedy", "greedy" or "UCB"
#' @param epsilon scalar numeric: ratio of random exploration in 
#' epsilon-greedy action selection
#' @param initial.value scalar numeric: initial values for the action
#'  values Q, set this to the maximal 
#'  possible reward to encourage exploration (optimistic initialization)
#' @param initial.visits scalar integer: set this to a high number to 
#' encourage exploration (together with a high initial.value)
#' @param epsilon.decay scalar numeric between 0 and 1: decay epsilon 
#' every 100 episodes by this factor
#' @param ... arguments passed on to action selection algorithm
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
#'   action.selection = "UCB", c = 2)
#' # true values: 1, 2, 2.5, 4
solveBandit = function(bandit, n.episodes = 10L,
  action.selection = c("epsilon-greedy", "greedy", "UCB"), epsilon = 0.1, 
  epsilon.decay = NULL, initial.value = 0, initial.visits = 0L, c = 2, ...) {
  
  action.selection <- match.arg(action.selection)
  action.visits = rep(initial.visits, bandit$n.actions)
  rewards = rep(initial.value * initial.visits, bandit$n.actions)
  Q = rep(initial.value, bandit$n.actions)
  
  for (i in seq_len(n.episodes)) {
    if (action.selection == "greedy") {
      action = argmax(Q) - 1
    }
    if (action.selection == "epsilon-greedy") {
      action = sample_epsilon_greedy_action(Q, epsilon)
    }
    if (action.selection == "UCB") {
      if (any(action.visits == 0)) {
        action = sample(which(action.visits == 0), 1) - 1
      } else {
       action = argmax(Q + c * sqrt(log(i) / action.visits)) - 1 
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
