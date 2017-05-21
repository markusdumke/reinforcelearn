# internal helper functions

# Q a numeric vector: the action value function for a given state
# epsilon numeric scalar in [0, 1]: probability of selecting a random action
# sample_epsilon_greedy_action(c(1, 2, 3), epsilon = 0.2)
sample_epsilon_greedy_action = function(Q, epsilon) {
  greedy_action = argmax(Q)
  random_actions = seq(1, length(Q))
  # non_greedy_actions = actions[actions != greedy_action]
  action = sample(c(greedy_action, random_actions), size = 1,  
    prob = c(1 - epsilon, rep(epsilon / length(random_actions), length(random_actions))))
  action - 1L
}

# returns probabilities of actions according to epsilon-greedy policy
# arguments same as above
returnPolicy = function(Q, epsilon) {
  greedy_action = argmax(Q)
  n.actions = length(Q)
  policy = rep(0, n.actions)
  policy[greedy_action] = 1 - epsilon
  policy = policy + epsilon / n.actions
  policy
}

# Argmax (ties broken randomly)
# x numeric matrix or numeric vector
argmax = function(x) {
  nnet::which.is.max(x)
}
