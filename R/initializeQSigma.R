qSigmaAgent = R6::R6Class(public = list(
  
  runEpisode = NULL,
  start = NULL,
  interactWithEnvironment = NULL,
  getOnlineData = NULL,
  preprocessState = NULL,
  
  getAction = NULL,
  sampleActionFromPolicy = NULL,
  getPolicy = NULL,
  predictQ = NULL,
  
  online.data = NULL,
  Q1 = NULL,
  policy = NULL,
  epsilon = NULL,
  episode.steps = NULL,
  
  getOldQ = NULL,
  getTrainData = NULL,
  train = NULL,
  fit = NULL,
  getTdTarget = NULL,
  getTdError = NULL,
  getSarsaTarget = NULL,
  getExpSarsaTarget = NULL,
  getNextAction = NULL,
  updateQ = NULL,
  train.data = NULL,
  sigma = NULL,
  td.target = NULL,
  td.error = NULL,
  discount = NULL,
  learning.rate = NULL,
  epsilon.target = NULL,
  next.action = NULL,
  getNextQ = NULL,
  Q.old = NULL,
  
  n.actions = NULL,
  
  Q.predict = NULL,
  Q.target = NULL,
  Q2 = NULL,
  updateTargetModel = NULL,
  updateTargetModel2 = NULL,
  
  replay.memory = NULL,
  replay.index = 0,
  alpha = NULL,
  
  sampleBatch = NULL,
  getIndices = NULL,
  priority = NULL,
  indices = NULL,
  add2ReplayMemory = NULL,
  batch.size = NULL,
  updatePriority = NULL,
  theta = NULL,
  
  E = NULL,
  increaseEligibility = NULL,
  resetEligibility = NULL,
  reduceEligibility = NULL,
  lambda = NULL,
  beta = NULL,
  
  initialize = function(envir, value.function, preprocessState, 
    model, initial.value, n.episodes, sigma, 
    target.policy, lambda, beta, learning.rate, 
    epsilon, discount, double.learning, update.target.after, 
    replay.memory, replay.memory.size, batch.size, alpha, theta, 
    updateEpsilon, updateSigma, updateLambda, updateAlpha, 
    updateLearningRate, updateTheta, printing) {
    
    # Algorithm depends on the following arguments:
    #-----------------------
    # type of function approximation
    # experience replay: yes / no -> prioritized experience replay: yes / no
    # double learning: yes / no
    # multi-step bootstrapping: lambda > 0 ?
    # sigma value: Sarsa / Q-Learning
    # update parameters over time?
    
    self$epsilon = epsilon
    if (target.policy == "e-greedy") {
      self$epsilon.target = epsilon
    } else {
      self$epsilon.target = 0
    }
    self$discount = discount
    self$learning.rate = learning.rate
    self$beta = beta
    self$sigma = sigma
    self$lambda = lambda
    self$alpha = alpha
    self$theta = theta
    
    self$n.actions = envir$n.actions
    self$episode.steps = rep(0, n.episodes)
    
    if (replay.memory.size > 1 | !is.null(replay.memory)) {
      experience.replay = TRUE
    } else {
      experience.replay = FALSE
    }
    if (!is.null(replay.memory)) {
      replay.memory.size = length(replay.memory)
    }
    
    if (alpha > 0) {
      prioritized.exp.replay = TRUE
    } else {
      prioritized.exp.replay = FALSE
    }
    
    # eligibility only used if no experience replay is used
    if (lambda > 0 & experience.replay == FALSE) {
      eligibility = TRUE
    } else {
      eligibility = FALSE
    }
    
    # # action value table
    # if (value.function == "table") {
    #   self$Q1 = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
    # }
    # 
    # # main function
    # self$runEpisode = function(envir, i) {
    #   self$start(envir)
    #   while(envir$done == FALSE) {
    #     self$interactWithEnvironment(envir)
    #     self$train()
    #     if (envir$done) {
    #       self$episode.steps[i] = envir$n.steps
    #       if (printing) {
    #         print(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
    #       }
    #       break
    #     }
    #   }
    # }
    # 
    # # at begin of episode
    # if (eligibility == FALSE) {
    #   self$start = function(envir) {
    #     envir$reset()
    #   }
    # }
    # 
    # state preprocessing
    if (is.null(preprocessState)) {
      self$preprocessState = identity
    } else {
      self$preprocessState = preprocessState
    }
    # 
    # # create list of current online data
    # self$getOnlineData = function(envir, action) {
    #   list(state = self$preprocessState(envir$previous.state), action = action,
    #     reward = envir$reward, next.state = self$preprocessState(envir$state))
    # }
    # 
    # # interaction agent-environment, get online data
    # self$interactWithEnvironment = function(envir) {
    #   s = self$preprocessState(envir$state)
    #   action = self$getAction(s)
    #   envir$step(action)
    #   self$online.data = self$getOnlineData(envir, action)
    # }
    # 
    # # get action given state
    # self$getAction = function(state) {
    #   Q.state = self$predictQ(self$Q1, state)
    #   self$getPolicy(Q.state, self$epsilon)
    #   action = self$sampleActionFromPolicy(self$policy)
    #   return(action)
    # }
    # 
    # 
    # # predict Q values for each action given state
    # if (value.function == "table") {
    #   self$predictQ = function(Q, state) {
    #     return(Q[state + 1, , drop = FALSE])
    #   }
    # }
    # 
    # if (value.function == "neural.network") {
    #   self$predictQ = function(Q, state) {
    #     predict(Q, state)
    #   }
    # }
    # 
    # # train model on training data
    # self$train = function() {
    #   self$getTrainData()
    #   self$getTdTarget()
    #   self$fit()
    # }
    # 
    # self$fit = function() {
    #   self$getTdError()
    #   self$updateQ()
    # }
    # 
    # # neural network without prioritized experience replay does not need to compute td error
    # if (value.function == "neural.network" & prioritized.exp.replay == FALSE) {
    #   self$fit = function() {
    #     self$updateQ()
    #   }
    # }
    # 
    # # when no experience replay use online data
    # if (experience.replay == FALSE) {
    #   self$getTrainData = function() {
    #     self$train.data = self$online.data
    #   }
    # }
    # 
    # if (experience.replay) {
    #   self$batch.size = batch.size
    #   
    #   # get training data from replay memory
    #   self$getTrainData = function() {
    #     self$replay.index =  self$replay.index + 1
    #     if (self$replay.index > replay.memory.size) {
    #       self$replay.index = 1
    #     }
    #     self$add2ReplayMemory(self$replay.index)
    #     self$train.data = self$sampleBatch()
    #   }
    #   
    #   # add current data to replay memory, replacing oldest entry
    #   self$add2ReplayMemory = function(index) {
    #     self$replay.memory[[index]] = self$online.data
    #   }
    #   
    #   # fill replay memory initially with experience from random uniform policy
    #   initializeReplayMemory = function(envir, len) {
    #     self$replay.memory = vector("list", length = len)
    #     envir$reset()
    #     action = sample(envir$actions, 1)
    #     for (i in seq_len(len)) {
    #       envir$step(action)
    #       self$online.data = self$getOnlineData(envir, action)
    #       self$add2ReplayMemory(i)
    #       action = sample(envir$actions, 1)
    #       if (envir$done) {
    #         envir$reset()
    #       }
    #     }
    #   }
    #   
    #   # initialize replay memory if no replay memory is supplied
    #   if (is.null(replay.memory)) {
    #     initializeReplayMemory(envir, replay.memory.size)
    #   } else {
    #     self$replay.memory = replay.memory
    #   }
    #   
    #   # uniform sampling from replay memory (unprioritized replay)
    #   self$getIndices = function(batch.size) {
    #     indices = sample(seq_along(self$replay.memory), size = batch.size)
    #     indices
    #   }
    #   
    #   # sampling with prioritized experience replay (proportional to td error)
    #   if (prioritized.exp.replay) {
    #     
    #     self$priority = rep(1, times = replay.memory.size)
    #     
    #     self$getIndices = function(batch.size) {
    #       probability = self$priority ^ self$alpha / sum(self$priority ^ self$alpha)
    #       self$indices = sample(seq_along(self$replay.memory), size = batch.size, prob = probability)
    #       self$indices
    #     }
    #     
    #     # update priority for sampled batch to be proportional to td error
    #     self$updatePriority = function() {
    #       self$priority[self$indices] = abs(self$td.error) + self$theta
    #     }
    #     
    #     # adjust train function to update priority
    #     self$train = function() {
    #       self$getTrainData()
    #       self$getTdTarget()
    #       self$fit()
    #       self$updatePriority()
    #     }
    #   }
    #   
    #   # sample batch from replay memory
    #   self$sampleBatch = function() {
    #     self$indices = self$getIndices(self$batch.size)
    #     batch = self$replay.memory[self$indices]
    #     states = lapply(batch, "[[", "state")
    #     next.states = lapply(batch, "[[", "next.state")
    #     actions = vapply(batch, "[[", "action", FUN.VALUE = integer(1))
    #     rewards = vapply(batch, "[[", "reward", FUN.VALUE = double(1))
    #     return(list(state = states, action = actions, reward = rewards,
    #       next.state = next.states))
    #   }
    # }
    # 
    # # depending on the value of sigma: compute sarsa target and / or expected sarsa target
    # self$getTdTarget = function() {
    #   self$getOldQ()
    #   self$getNextQ()
    #   sarsa.target = self$getSarsaTarget()
    #   exp.sarsa.target = self$getExpSarsaTarget()
    #   self$td.target = self$train.data$reward + self$discount * (self$sigma * sarsa.target +
    #       (1 - self$sigma) * exp.sarsa.target)
    # }
    # 
    # if (sigma == 1 & is.null(updateSigma)) {
    #   self$getTdTarget = function() {
    #     self$getOldQ()
    #     self$getNextQ()
    #     self$td.target = self$train.data$reward + self$discount * self$getSarsaTarget()
    #   }
    # }
    # 
    # if (sigma == 0 & is.null(updateSigma)) {
    #   self$getTdTarget = function() {
    #     self$getOldQ()
    #     self$getNextQ()
    #     self$td.target = self$train.data$reward + self$discount * self$getExpSarsaTarget()
    #   }
    # }
    # 
    # # get next action due to current policy
    # if (experience.replay) {
    #   self$getNextAction = function() {
    #     next.action = vapply(self$train.data$next.state, self$getAction, FUN.VALUE = integer(1))
    #     return(next.action)
    #   }
    # } else {
    #   self$getNextAction = function() {
    #     next.action = self$getAction(self$train.data$next.state)
    #     return(next.action)
    #   }
    # }
    # 
    # self$getSarsaTarget = function() {
    #   self$next.action = self$getNextAction()
    #   return(self$Q.target[matrix(c(seq_len(nrow(self$Q.target)), self$next.action + 1), ncol = 2)])
    # }
    # 
    # self$getExpSarsaTarget = function() {
    #   policy = t(apply(self$Q.predict, 1, self$getPolicy, epsilon = self$epsilon.target))
    #   exp.sarsa.target = rowSums(policy * self$Q.target)
    #   return(exp.sarsa.target)
    # }
    # 
    # if (!double.learning) {
    #   if (experience.replay) {
    #     self$getNextQ = function() {
    #       self$Q.predict = t(vapply(self$train.data$next.state, self$predictQ, Q = self$Q1,
    #         FUN.VALUE = numeric(self$n.actions)))
    #       self$Q.target = self$Q.predict
    #     }
    #   } else {
    #     self$getNextQ = function() {
    #       self$Q.predict = self$predictQ(Q = self$Q1, self$train.data$next.state)
    #       self$Q.target = self$Q.predict
    #     }
    #   }
    # }
    # 
    # # Q values of old state
    # if (experience.replay) {
    #   self$getOldQ = function() {
    #     self$Q.old = t(vapply(self$train.data$state, self$predictQ, Q = self$Q1,
    #       FUN.VALUE = numeric(self$n.actions)))
    #   }
    # } else {
    #   self$getOldQ = function() {
    #     self$Q.old = self$predictQ(self$Q1, self$train.data$state)
    #   }
    # }
    # 
    # 
    # self$getTdError = function() {
    #   self$td.error = self$td.target - self$Q.old[matrix(c(seq_len(nrow(self$Q.old)), self$train.data$action + 1), ncol = 2)]
    # }
    # 
    # if (value.function == "table") {
    #   self$updateQ = function() {
    #     state = unlist(self$train.data$state)
    #     self$Q1[matrix(c(state + 1, self$train.data$action + 1), ncol = 2)] = 
    #       self$Q1[matrix(c(state + 1, self$train.data$action + 1), ncol = 2)] + self$learning.rate * self$td.error
    #   }
    # }
    # 
    # if (double.learning) {
    #   self$getNextQ = function() {
    #     self$Q.predict = t(vapply(self$train.data$next.state, self$predictQ, Q = self$Q1,
    #       FUN.VALUE = numeric(self$n.actions)))
    #     self$Q.target = t(vapply(self$train.data$next.state, self$predictQ, Q = self$Q2,
    #       FUN.VALUE = numeric(self$n.actions)))
    #   }
    # }
    # 
    # # with eligibility traces
    # if (eligibility & value.function == "table") {
    #   
    #   self$start = function(envir) {
    #     self$resetEligibility(envir)
    #     envir$reset()
    #   }
    #   
    #   self$resetEligibility = function(envir) {
    #     self$E = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
    #   }
    #   
    #   self$increaseEligibility = function() {
    #     self$E[self$online.data$state + 1, self$online.data$action + 1] = (1 - self$beta) *
    #       self$E[self$online.data$state + 1, self$online.data$action + 1] + 1
    #   }
    #   
    #   self$reduceEligibility = function(policy, next.action) {
    #     self$E = self$discount * self$lambda * self$E *
    #       (self$sigma + self$policy[self$next.action + 1] * (1 - self$sigma))
    #   }
    #   
    #   self$updateQ = function() {
    #     self$Q1 = self$Q1 + self$learning.rate * self$td.error * self$E
    #   }
    #   
    #   self$train = function() {
    #     self$getTrainData()
    #     self$increaseEligibility()
    #     self$getTdTarget()
    #     self$getTdError()
    #     self$updateQ()
    #     self$reduceEligibility()
    #   }
    # }
    # 
    # if (double.learning) {
    #   self$Q2 = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
    #   
    #   self$updateTargetModel = function(steps, update.target.after) {
    #     if (steps %% update.target.after == 0) {
    #       self$updateTargetModel2()
    #     }
    #   }
    #   
    #   self$updateTargetModel2 = function() {
    #     self$Q2 = self$Q1
    #   }
    # }
    # 
    # # for neural network as function approximator
    # if (value.function == "neural.network") {
    #   keras::compile(model, loss = 'mse', optimizer = keras::optimizer_sgd(lr = self$learning.rate))
    #   self$Q1 = model
    #   
    #   if (experience.replay) {
    #     self$updateQ = function() {
    #       self$Q.old[matrix(c(seq_len(nrow(self$Q.old)), self$train.data$action + 1), ncol = 2)] = self$td.target
    #       states = Reduce(rbind, self$train.data$state)
    #       keras::fit(self$Q1, states, self$Q.old, verbose = 0, epochs = 1, batch_size = self$batch.size)
    #     }
    #   } else {
    #     self$updateQ = function() {
    #       self$Q.old[matrix(c(seq_len(nrow(self$Q.old)), self$train.data$action + 1), ncol = 2)] = self$td.target
    #       keras::fit(self$Q1, self$train.data$state, self$Q.old, verbose = 0, epochs = 1, batch_size = 1)
    #     }
    #   }
    #   
    #   
    #   # transfer weights from online neural network to target neural network
    #   if (double.learning){
    #     self$Q2 = model
    #     self$updateTargetModel2 = function() {
    #       keras::set_weights(self$Q2, keras::get_weights(self$Q1))
    #     }
    #   }
    # }
    # 
    # # double learning: use two value functions
    # if (double.learning) {
    #   self$getNextQ = function() {
    #     self$Q.predict = self$predictQ(self$Q1, self$train.data$next.state)
    #     self$Q.target = self$predictQ(self$Q2, self$train.data$next.state)
    #   }
    #   
    #   if (prioritized.exp.replay) {
    #     self$train = function() {
    #       self$getTrainData()
    #       self$getTdTarget()
    #       self$fit()
    #       self$updatePriority()
    #       self$updateTargetModel(envir$n.steps, update.target.after)
    #     }
    #   } else {
    #     self$train = function() {
    #       self$getTrainData()
    #       self$getTdTarget()
    #       self$fit()
    #       self$updateTargetModel(envir$n.steps, update.target.after)
    #     }
    #   }
    # }
    
    # ---- Linear Function Approximation
    if (value.function == "linear") {
      envir$reset()
      n.weights = length(preprocessState(envir$state))
      self$Q1 = matrix(0, #runif(n.weights * envir$n.actions),
        nrow = n.weights, ncol = envir$n.actions)
      
      self$predictQ = function(Q, state) {
        state %*% Q
      }
      
      if (!experience.replay) {
        if (!eligibility) {
          self$runEpisode = function(envir, i) {
            envir$reset()
            s = self$preprocessState(envir$state)
            Q = self$predictQ(self$Q1, s)
            policy = getPolicy(Q, self$epsilon)
            a = sampleActionFromPolicy(policy)
            
            while(envir$done == FALSE) {
              envir$step(a)
              
              s.n = self$preprocessState(envir$state)
              Q.n = self$predictQ(self$Q1, s.n)
              policy = getPolicy(Q.n, self$epsilon)
              a.n = sampleActionFromPolicy(policy)
              
              if (target.policy == "greedy") {
                policy = getPolicy(Q.n, self$epsilon.target)
              }
              
              sarsa.target = Q.n[a.n + 1]
              exp.sarsa.target = sum(policy * Q.n)
              td.target = envir$reward + discount * (sigma * sarsa.target + 
                  (1 - sigma) * exp.sarsa.target)
              td.error = td.target - Q[a + 1]
              self$Q1[, a + 1] = self$Q1[, a + 1] + self$learning.rate * td.error * s
              
              s = s.n
              a = a.n
              Q = Q.n
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
                if (printing) {
                  print(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                }
                break
              }
            }
          }
        } else { # if eligibility
          self$runEpisode = function(envir, i) {
            self$E = matrix(0, nrow = nrow(self$Q1), ncol = envir$n.actions)
            envir$reset()
            s = self$preprocessState(envir$state)
            Q = self$predictQ(self$Q1, s)
            policy = getPolicy(Q, self$epsilon)
            a = sampleActionFromPolicy(policy)
            
            while(envir$done == FALSE) {
              envir$step(a)
              
              s.n = self$preprocessState(envir$state)
              Q.n = self$predictQ(self$Q1, s.n)
              policy = getPolicy(Q.n, self$epsilon)
              a.n = sampleActionFromPolicy(policy)
              
              self$E[, a + 1] = (1 - self$beta) * self$E[, a + 1] + s
              
              if (target.policy == "greedy") {
                policy = getPolicy(Q.n, self$epsilon.target)
              }
              
              sarsa.target = Q.n[a.n + 1]
              exp.sarsa.target = sum(policy * Q.n)
              td.target = envir$reward + discount * (sigma * sarsa.target + 
                  (1 - sigma) * exp.sarsa.target)
              td.error = td.target - Q[a + 1]
              
              self$Q1 = self$Q1 + self$learning.rate * td.error * self$E
              
              self$E = self$discount * self$lambda * self$E *
                (self$sigma + policy[a.n + 1] * (1 - self$sigma))
              
              s = s.n
              a = a.n
              Q = Q.n
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
                if (printing) {
                  print(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                }
                break
              }
            }
          }
        }
      }
      
      # if (double.learning) {
      #   self$Q2 = matrix(0, nrow = n.weights, ncol = envir$n.actions)
      # }
      
    } # exp. replay, double learning, binary features
    
    # if update Params
    
  }
)
)

# get epsilon-greedy policy with respect to Q 
getPolicy = function(Q, epsilon) {
  greedy.action = which.max(Q)
  n.actions = length(Q)
  policy = matrix(0, nrow = 1, ncol = n.actions)
  policy[, greedy.action] = 1 - epsilon
  return(policy + epsilon / n.actions)
}

# sample action from policy
sampleActionFromPolicy = function(policy) {
  return(sample(seq_len(ncol(policy)), prob = policy, size = 1, replace = TRUE) - 1L)
}
