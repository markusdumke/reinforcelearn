# internal helper functions

# # Dynamic Programming: Evaluate a policy
# evaluatePolicy2 = function(policy, envir, v, non.terminal.states, discount.factor, precision) {
#   v.new = v
#   P = matrix(envir$transition.array, nrow = envir$n.states, ncol = envir$n.actions * envir$n.states)
#   R = envir$reward.matrix
#   improvement = TRUE
#   
#   while (improvement == TRUE) {
#     v2 = Matrix::bdiag(v, v, v, v)
#     d = matrix(policy * (R + (P %*% v2)), nrow = envir$n.states, ncol = envir$n.actions)
#     v.new = rowSums(d)
#     improvement = any(abs(v - v.new) > precision)
#     v = v.new
#   }
#   v
# }

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
