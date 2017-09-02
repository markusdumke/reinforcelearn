# internal helper functions

# Dynamic Programming: Evaluate a policy
# evaluatePolicy2 = function(envir, policy, v, non.terminal.states, discount, precision) {
#   improvement = TRUE
#   Q = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
#   while (improvement == TRUE) {
#     for (i in seq_len(envir$n.actions)) {
#       Q[non.terminal.states + 1, i] = policy[non.terminal.states + 1, i] * 
#         (envir$reward.matrix[non.terminal.states + 1, i] + 
#         discount * envir$transition.array[non.terminal.states + 1, non.terminal.states + 1, i] %*% 
#             v[non.terminal.states + 1])
#     }
#     v.new = rowSums(Q)
#     improvement = any(abs(v - v.new) > precision)
#     v = v.new
#   }
#   list(v = v, Q = Q)
# }

sampleActionBandit = function(Q, epsilon) {
  greedy.action = argmax(Q)
  random.actions = seq(1, length(Q))
  # non.greedy.actions = actions[actions != greedy.action]
  action = sample(c(greedy.action, random.actions), size = 1,
    prob = c(1 - epsilon, rep(epsilon / length(random.actions), length(random.actions))))
  action - 1L
}

# returnPolicy = function(Q, epsilon = 0) {
#   greedy.action = argmax(Q) # apply(Q, 1, argmax)
#   n.actions = length(Q)
#   policy = matrix(0, nrow = 1, ncol = n.actions)
#   policy[matrix(c(seq_len(n.actions), greedy.action), ncol = 2)] = 1 - epsilon
#   policy + epsilon / n.actions
# }

sampleAction2 = function(policy) {
  sample(seq_len(ncol(policy)), prob = policy[1, ], size = 1) - 1L
}

# Argmax (ties broken randomly)
# x numeric matrix or numeric vector
argmax = function(x) {
  nnet::which.is.max(x)
}

# sampleEpisode = function(policy, envir, initial.state = NULL, initial.action = NULL) {
#   rewards = numeric(0)
#   if (!is.null(initial.action)) {
#     actions = initial.action
#   } else {
#     actions = integer(0)
#   }
#   if (!is.null(initial.state)) {
#     states = initial.state
#     envir$state = initial.state
#   } else {
#     envir$reset()
#     states = envir$state
#   }
#   
#   i = 1
#   
#   while (envir$done == FALSE) {
#     actions = append(actions, sample(envir$actions, prob = policy[states[i], ], size = 1))
#     envir$step(actions[i])
#     states = append(states, envir$state)
#     rewards = append(rewards, envir$reward)
#     i = i + 1
#   }
#   list(states = states, actions = c(actions, NA), rewards = c(NA, rewards))
# }

# # Estimate return
# estimateReturn = function(rewards, discount) {
#   sum(discount ^ seq(0, length(rewards) - 1) * rewards)
# }
