#' Monte Carlo Control with Exploring Starts
#'
#' @inheritParams predictMC
#' @inheritParams evaluatePolicy
#'
#' @return the optimal policy and optimal action value function Q
#' @import dplyr
#' @examples 
#' set.seed(123)
#' grid = gridworld$new()
#'   
#' # Estimate optimal action value function with Monte Carlo Exploring Starts
#' # Q = MonteCarloES(grid)
MonteCarloES <- function(envir, n.episodes = 100, discount.factor = 1) {
  
  # input checking
  print("Currently not implemented.")
  
  # # initialize action value function Q(s, a), policy pi and
  # #   counter for number of visits to each state-action pair randomly
  # n.states = envir$n.states
  # n.actions = envir$n.actions
  # Q = matrix(0, nrow = n.states, ncol = n.actions)
  # policy = matrix(1 / n.actions, nrow = n.states, ncol = n.actions)
  # n.visits = matrix(0, nrow = n.states, ncol = n.actions)
  # # ----------------------------------------
  # 
  # # for each episode:
  # #  sample initial state and initial action 
  # #   (exploring starts: all pairs must have probability > 0)
  # for (i in seq_len(n.episodes)) {
  #   state = sample(envir$non.terminal.states, size = 1)
  #   action = sample(envir$actions, size = 1)
  #   
  #   # sample episode: list of states, actions, rewards
  #   envir$setEpisodeOverFalse()
  #   episode = sampleEpisode(policy, envir, initial.state = state, 
  #     initial.action = action)
  #   
  #   # estimate return for all visited state-action pairs with the 
  #   #  Monte Carlo first-visit method
  #   
  #   # get unique visited state-action pairs
  #   state_action_pairs = dplyr::distinct(data.frame(episode$states, episode$actions))
  #   
  #   # find first occurences
  #   first.occurence = match(state_action_pairs, cbind(episode$states, episode$actions))
  #   
  #   # increment counter for all visited state-action pairs
  #   n.visits[state_action_pairs] <- n.visits[state_action_pairs] + 1
  #   
  #   # estimate returns for all these state-action-pairs using MC first-visit
  #   for (j in seq_len(nrow(state_action_pairs))) {
  #     # first.occurence = match(episode) # min(which(episode$states == j[, 1] & j[, 2]))
  #     sequ = seq(first.occurence[j], length(episode$rewards))
  #     rewards = episode$rewards[sequ]
  #     G = estimateReturn(rewards, discount.factor)
  #   }
  #   
  #   # average returns (incremental mean)
  #   Q = Q + 1 / n.visits * (G - Q)
  #   
  #   # make greedy_policy update for these pairs
  #   policy = make_greedy_policy(Q)
  # }
  # 
  # list(action_value_function = Q, optimal_policy = policy)
}

# Make greedy policy, ties are not broken at random!
make_greedy_policy <- function(Q) {
  argmax = apply(Q, 1, which.max)
  policy = matrix(0, ncol = ncol(Q), nrow = nrow(Q)) # inefficient?
  policy[cbind(seq(1, nrow(policy)), argmax)] = 1
  policy
}






# # @param episode a list of states, actions, returns for one episode
# # @param n.visits count number of visits to state-action pairs
# first_visitMC = function(states, actions, rewards, discount.factor) {
#   
#   # implement first-visit MC
#   
# }

# Estimate return following the first occurence of given state-action pair
# states, actions, rewards must have same length
# estimate_return_firstvisit_one_state_action_pair <- function(state, action, rewards, states, actions, discount.factor) {
#   first.occurence = min(which(states == state & actions == action))
#   sequ = seq(first.occurence, length(states))
#   
#   estimateReturn(rewards[sequ], discount.factor)
# }

