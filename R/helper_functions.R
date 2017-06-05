# internal helper functions

# Dynamic Programming: Evaluate a policy
evaluatePolicy2 = function(policy, envir, v, non.terminal.states, discount.factor, precision) {
  improvement = TRUE
  v.new = v
  v2 = matrix(0, ncol = envir$n.actions, nrow = envir$n.states)
  while (improvement == TRUE) {
    for (i in seq_len(envir$n.actions)) {
      v2[non.terminal.states + 1, i] = policy[non.terminal.states + 1, i] * 
        (envir$reward.matrix[non.terminal.states + 1, i] + 
        discount.factor * envir$transition.array[non.terminal.states + 1, non.terminal.states + 1, i] %*% 
            v[non.terminal.states + 1])
    }
    v.new = rowSums(v2)
    improvement = any(abs(v - v.new) > precision)
    v = v.new
  }
  v
}

# Dynamic Programming: Policy Improvement by acting greedily with respect to V
improvePolicy = function(v, envir, discount.factor) {
  # multiply each transition matrix for each action P[, ,  i] 
  #   with reward plus discounted value of next state
  Q = apply(envir$transition.array, 3, function(x) 
    rowSums(x %*% (envir$reward.matrix + discount.factor * v)))
  greedy.actions = apply(Q, 1, argmax)
  policy = matrix(0, nrow = nrow(Q), ncol = ncol(Q))
  policy[matrix(c(seq_len(envir$n.states), greedy.actions), ncol = 2)] = 1
  policy
}


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
