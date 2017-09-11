#' Solve multi-armed bandit
#'
#' Multi-armed bandits are a simplified reinforcement learning problem,
#' each arm of the bandit pays off a reward and the goal is to maximize
#' this reward, i.e. to choose the best arm. The arms of the bandit
#' can be seen as actions, after each action the episode ends (there
#' are no states). To find the best action, the algorithm is faced with
#' a tradeoff between exploration and exploitation.
#'
#' @param n.episodes [\code{integer(1)}] \cr
#'   Number of episodes.
#' @param action.selection [\code{character(1)}] \cr
#'   Which method to use for action selection, one of \code{"epsilon-greedy"},
#'   \code{"greedy"}, \code{"UCB"} or \code{"gradient_bandit"}.
#' @param epsilon [\code{numeric(1) in [0,1]}] \cr
#'   Ratio of random exploration in epsilon-greedy action selection.
#' @param initial.value [\code{numeric(1)}] \cr
#'   Initial values for the action values Q, set this to the maximal possible
#'   reward to encourage exploration (optimistic initialization).
#' @param initial.visits [\code{integer(1)}] \cr
#'   Set this to a high number to encourage exploration
#'   (together with a high \code{initial.value}).
#' @param epsilon.decay [\code{numeric(1) in [0,1]}] \cr
#'   Decay epsilon by this factor.
#' @param epsilon.decay.after [\code{integer(1)}] \cr
#'   Number of episodes after which to decay epsilon.
#' @param C [\code{numeric(1)}] \cr
#'   Controls the degree of exploration. High C values lead to more exploration.
#' @param step [\code{function}] \cr
#'   A function, which takes an action (\code{integer(1)}) as first argument
#'   and returns a numeric scalar reward.
#' @param n.actions [\code{integer(1)}] \cr
#'   Number of actions.
#' @param alpha [\code{numeric(1)}] \cr
#'   Parameter of gradient bandit algorithm, higher alpha value gives more weight to
#'   recent rewards (useful for non-stationary environments). Not implemented!
#'
#' @return [\code{numeric}] \cr
#' Returns the action values for the arms of the bandit or,
#' for gradient-bandit action selection, the probabilities for
#' each action.
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' # Define reward function
#' step = function(action) {
#'   if (action == 1) {
#'     reward = rnorm(1, mean = 1, sd = 1)
#'   }
#'   if (action == 2) {
#'     reward = rnorm(1, mean = 2, sd = 4)
#'   }
#'   if (action == 3) {
#'     reward = runif(1, min = 0, max = 5)
#'   }
#'   if (action == 4) {
#'     reward = rexp(1, rate = 0.25)
#'   }
#'   reward
#' }
#'
#' solveBandit(step, n.actions = 4, n.episodes = 1000,
#'   action.selection = "greedy")
#' solveBandit(step, n.actions = 4, n.episodes = 1000,
#'   action.selection = "egreedy", epsilon = 0.5)
#' solveBandit(step, n.actions = 4, n.episodes = 1000,
#'   action.selection = "greedy",
#'   initial.value = 5, initial.visits = 100)
#' solveBandit(step, n.actions = 4, n.episodes = 1000,
#'   action.selection = "UCB", C = 2)
#' # true values: 1, 2, 2.5, 4
#'
#' # Gradient bandit algorithm
#' solveBandit(step, n.actions = 4, n.episodes = 10000,
#'   action.selection = "gradientbandit", alpha = 0.1)
solveBandit = function(step, n.actions, n.episodes = 100,
  action.selection = c("egreedy", "greedy", "UCB", "gradientbandit"),
  epsilon = 0.1, epsilon.decay = 0.5, epsilon.decay.after = 100,
  alpha = 0.1, initial.value = 0, initial.visits = 0, C = 2) {
  
  checkmate::assertFunction(step)
  checkmate::assertInt(n.actions)
  checkmate::assertInt(n.episodes, lower = 1)
  checkmate::assertChoice(action.selection, c("egreedy", "greedy", "UCB", "gradientbandit"))
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
    if (any(action.visits == 0)) {
      action = sample(which(action.visits == 0), 1)
    } else {
      if (action.selection == "greedy") {
        action = which.max(Q)
      } else if (action.selection == "egreedy") {
        if (i %% epsilon.decay.after == 0) {
          epsilon = epsilon * epsilon.decay
        }
        action = selectActionEgreedy(Q, n.actions, epsilon)
      } else if (action.selection == "UCB") {
        action = which.max(Q + sqrt(C * log(i) / action.visits))
      } else if (action.selection == "gradientbandit") {
        Q = exp(H) / sum(exp(H))
        action = sample(seq_len(n.actions), 1, prob = Q)
      }
    }
    reward = step(action)
    
    action.visits[action] = action.visits[action] + 1
    rewards[action] = rewards[action] + reward
    if (action.selection != "gradientbandit") {
      Q[action] = rewards[action] / action.visits[action]
    } else {
      total.reward = sum(rewards)
      total.action.visits = sum(action.visits)
      average.reward = total.reward / total.action.visits
      H[- action] = H[- action] - alpha * (reward - average.reward) * Q[ - action]
      H[action] = H[action] + alpha * 
        (reward - average.reward) * (1 - Q[action])
    }
  }
  Q
}

selectActionEgreedy = function(Q, n.actions, epsilon) {
  greedy.action = which.max(Q)
  action = sample(c(seq_len(n.actions), greedy.action), size = 1,
    prob = c(1 - epsilon, rep(epsilon / n.actions, n.actions)))
  action
}
