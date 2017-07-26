# internal helper functions

# Dynamic Programming: Evaluate a policy
# evaluatePolicy2 = function(envir, policy, v, non.terminal.states, discount.factor, precision) {
#   improvement = TRUE
#   Q = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
#   while (improvement == TRUE) {
#     for (i in seq_len(envir$n.actions)) {
#       Q[non.terminal.states + 1, i] = policy[non.terminal.states + 1, i] * 
#         (envir$reward.matrix[non.terminal.states + 1, i] + 
#         discount.factor * envir$transition.array[non.terminal.states + 1, non.terminal.states + 1, i] %*% 
#             v[non.terminal.states + 1])
#     }
#     v.new = rowSums(Q)
#     improvement = any(abs(v - v.new) > precision)
#     v = v.new
#   }
#   list(v = v, Q = Q)
# }

# Dynamic Programming: Policy Improvement by acting greedily with respect to V
improvePolicy = function(Q) {
  # multiply each transition matrix for each action P[, ,  i] 
  #   with reward plus discounted value of next state
  greedy.actions = apply(Q, 1, argmax)
  policy = matrix(0, nrow = nrow(Q), ncol = ncol(Q))
  policy[matrix(c(seq_len(nrow(Q)), greedy.actions), ncol = 2)] = 1
  policy
}


# Q a numeric vector: the action value function for a given state
# epsilon numeric scalar in [0, 1]: probability of selecting a random action
# sampleAction(c(1, 2, 3), epsilon = 0.2)
sampleAction = function(Q, epsilon) {
  greedy_action = argmax(Q)
  random_actions = seq(1, length(Q))
  # non_greedy_actions = actions[actions != greedy_action]
  action = sample(c(greedy_action, random_actions), size = 1,  
    prob = c(1 - epsilon, rep(epsilon / length(random_actions), length(random_actions))))
  action - 1L
}

# returns probabilities of actions according to epsilon-greedy policy
# Q: matrix
returnPolicy = function(Q, epsilon = 0) {
  greedy.actions = apply(Q, 1, argmax)
  policy = matrix(0, nrow = nrow(Q), ncol = ncol(Q))
  policy[matrix(c(seq_len(nrow(Q)), greedy.actions), ncol = 2)] = 1 - epsilon
  policy + epsilon / ncol(Q)
}

# Argmax (ties broken randomly)
# x numeric matrix or numeric vector
argmax = function(x) {
  nnet::which.is.max(x)
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
  
  while (envir$done == FALSE) {
    actions = append(actions, sample(envir$actions, prob = policy[states[i], ], size = 1))
    envir$step(actions[i])
    states = append(states, envir$state)
    rewards = append(rewards, envir$reward)
    i = i + 1
  }
  list(states = states, actions = c(actions, NA), rewards = c(NA, rewards))
}

# Estimate return
estimateReturn = function(rewards, discount.factor) {
  sum(discount.factor ^ seq(0, length(rewards) - 1) * rewards)
}
