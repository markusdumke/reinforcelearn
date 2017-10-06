# REINFORCE
reinforce = function(envir, n.episodes = 100, discount = 1, learning.rate = 0.01) {
  
  # start with random policy
  policy = matrix(1 / envir$n.actions, nrow = envir$n.states, ncol = envir$n.actions)
  # preferences for each action (table-lookup)
  h = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  
  for (i in seq_len(n.episodes)) {
    states = c()
    rewards = c()
    actions = c()
    envir$reset()
    
    while (envir$done == FALSE) {
      s = envir$state
      states = append(states, s)
      action = sampleActionFromPolicy(policy[s + 1, ])
      actions = append(actions, action)
      envir$step(action)
      rewards = append(rewards, envir$reward)
      
      if (envir$done) {
        print(paste("Episode", i, "finished after", envir$n.steps, "steps."))
        # update policy
        for (i in seq_along(states)) {
          G = sum(rewards[i:length(states)] * discount^(0:(length(states) - i)))
          h[states[i], actions[i]] = h[states[i], actions[i]] + 
            learning.rate * discount^(i - 1) * G * (1 - policy[states[i], actions[i]])
        }
        # softmax policy for discrete actions
        policy = exp(h) / rowSums(exp(h))
        #print(policy)
        break
      }
    }
  }
  policy
}

# REINFORCE + baseline
reinforceBase = function(envir, n.episodes = 100, discount = 1, alpha = 0.01, beta = 0.1) {
  
  # start with random policy
  policy = matrix(1 / envir$n.actions, nrow = envir$n.states, ncol = envir$n.actions)
  # preferences for each action (table-lookup)
  h = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  v = rep(0, envir$n.states)
  
  for (i in seq_len(n.episodes)) {
    states = c()
    rewards = c()
    actions = c()
    envir$reset()
    
    while (envir$done == FALSE) {
      s = envir$state
      states = append(states, s)
      action = sampleActionFromPolicy(policy[s + 1, ])
      actions = append(actions, action)
      envir$step(action)
      rewards = append(rewards, envir$reward)
      
      if (envir$done) {
        print(paste("Episode", i, "finished after", envir$n.steps, "steps."))
        # update policy
        for (i in seq_along(states)) {
          G = sum(rewards[i:length(states)] * discount^(0:(length(states) - i)))
          delta = G - v[states[i]]
          v[states[i] + 1] = v[states[i] + 1] + beta * delta
          h[states[i] + 1, actions[i] + 1] = h[states[i] + 1, actions[i] + 1] + 
            alpha * discount^(i - 1) * delta * (1 - policy[states[i] + 1, actions[i] + 1])
        }
        # softmax policy for discrete actions
        policy = exp(h) / rowSums(exp(h))
        #print(policy)
        break
      }
    }
  }
  policy
}

actorCritic = function(envir, n.episodes = 100, discount = 1, alpha = 0.01, beta = 0.1) {
  
  # start with random policy
  policy = matrix(1 / envir$n.actions, nrow = envir$n.states, ncol = envir$n.actions)
  # preferences for each action (table-lookup)
  h = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  v = rep(0, envir$n.states)
  
  for (i in seq_len(n.episodes)) {
    envir$reset()
    j = 1
    while(envir$done == FALSE) {
      s = envir$state
      action = sampleActionFromPolicy(policy[s + 1, ])
      envir$step(action)
      r = envir$reward
      print(r)
      s_n = envir$state
      
      delta = r + discount * v[s_n + 1] - v[s + 1]
      # if (envir$done) {
      #   delta = r - v[s + 1]
      # }
      v[s + 1] = v[s + 1] + beta * delta
      h[s + 1, ] = h[s + 1, ] + alpha * j * delta * (makeNHot(action + 1, envir$n.actions) - policy[s + 1, ])
      # softmax policy for discrete actions
      policy = exp(h) / rowSums(exp(h))
      j = discount * j
      if (envir$done) {
        print(paste("Episode", i, "finished after", envir$n.steps, "steps."))
        break
      }
    }
  }
  policy 
}


# env = makeGridworld(shape = c(4, 12), goal.states = 47,
#   cliff.states = 37:46, reward.step = - 1, reward.cliff = - 100,
#   cliff.transition.done = TRUE, initial.state = 36)
# 
# actorCritic(env, n.episodes = 500, discount = 1)
# reinforceBase(env, n.episodes = 500, discount = 1)

# Q Actor Critic
# Advantage Actor Critic
# TD Actor Critic
# TD lambda Actor Critic
