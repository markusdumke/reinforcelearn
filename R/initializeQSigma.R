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
  eligibility.type = NULL,
  
  initialize = function(envir, fun.approx, preprocessState, 
    model, initial.value, n.states, n.episodes, sigma, 
    target.policy, lambda, eligibility.type, learning.rate, 
    epsilon, discount, double.learning, update.target.after, 
    replay.memory, replay.memory.size, batch.size, alpha, theta, 
    updateEpsilon, updateSigma, updateLambda, updateAlpha, 
    updateLearningRate) {
    
    # Algorithm depends on the following arguments:
    #-----------------------
    # type of function approximation
    # experience replay: yes / no -> prioritized experience replay: yes / no
    # double learning: yes / no
    # multi-step bootstrapping: lambda > 0 ?
    # sigma value: Sarsa / Q-Learning
    # update parameters over time?
    
    self$epsilon = epsilon
    if (target.policy == "egreedy") {
      self$epsilon.target = epsilon
    } else {
      self$epsilon.target = 0
    }
    self$learning.rate = learning.rate
    self$eligibility.type = eligibility.type
    self$sigma = sigma
    self$lambda = lambda
    self$alpha = alpha
    
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
    
    # state preprocessing
    self$preprocessState = preprocessState
    
    if (experience.replay) {
      # initialize replay memory if no replay memory is supplied
      if (is.null(replay.memory)) {
        self$replay.memory = initializeReplayMemory(envir, replay.memory.size, self$preprocessState)
      } else {
        self$replay.memory = replay.memory
      }
      
      self$add2ReplayMemory = function(envir, action) {
        self$replay.index =  self$replay.index + 1
        if (self$replay.index > replay.memory.size) {
          self$replay.index = 1
        }
        data = list(state = self$preprocessState(envir$previous.state), action = action,
          reward = envir$reward, next.state = self$preprocessState(envir$state))
        self$replay.memory[[self$replay.index]] = data
        self$replay.memory
      }
      
      # uniform sampling from replay memory (unprioritized replay)
      self$getIndices = function(replay.memory.size, batch.size) {
        indices = sample(seq_len(replay.memory.size), size = batch.size)
        indices
      }
      
      # sampling with prioritized experience replay (proportional to td error)
      if (prioritized.exp.replay) {
        self$priority = rep(1, times = replay.memory.size)
        
        self$getIndices = function(replay.memory.size, batch.size) {
          probability = self$priority ^ self$alpha / sum(self$priority ^ self$alpha)
          indices = sample(seq_len(replay.memory.size), size = batch.size, prob = probability)
          indices
        }
        
        # update priority for sampled batch to be proportional to td error
        self$updatePriority = function(td.error, theta) {
          self$priority[self$indices] = abs(td.error) + theta
          self$priority
        }
      } else {
        self$updatePriority = function(td.error, theta) {}
      }
      
      
      # sample batch from replay memory
      self$sampleBatch = function() {
        self$indices = self$getIndices(replay.memory.size, batch.size)
        batch = self$replay.memory[self$indices]
        states = lapply(batch, "[[", "state")
        next.states = lapply(batch, "[[", "next.state")
        actions = vapply(batch, "[[", "action", FUN.VALUE = integer(1))
        rewards = vapply(batch, "[[", "reward", FUN.VALUE = double(1))
        return(list(state = states, action = actions, reward = rewards,
          next.state = next.states))
      }
      
      if (fun.approx == "table") {
        self$sampleBatch = function() {
          self$indices = self$getIndices(replay.memory.size, batch.size)
          batch = self$replay.memory[self$indices]
          states = vapply(batch, "[[", "state", FUN.VALUE = double(1))
          next.states = vapply(batch, "[[", "next.state", FUN.VALUE = double(1))
          actions = vapply(batch, "[[", "action", FUN.VALUE = integer(1))
          rewards = vapply(batch, "[[", "reward", FUN.VALUE = double(1))
          return(list(state = states, action = actions, reward = rewards,
            next.state = next.states))
        }
      }
    }
    
    # ---- Tabular Value Function
    if (fun.approx == "table") {
      
      if (!is.null(n.states)) {
        n.states = n.states
      } else {
        n.states = envir$n.states
      }
      
      if (is.null(initial.value)) {
        self$Q1 = matrix(0, nrow = n.states, ncol = envir$n.actions)
      } else {
        self$Q1 = initial.value
      }
      
      self$predictQ = function(Q, state) {
        Q[state + 1, , drop = FALSE]
      }
      
      if (!experience.replay) {
        if (!eligibility) {
          self$runEpisode = function(envir, i) {
            envir$reset()
            s = self$preprocessState(envir$state)
            Q = self$predictQ(self$Q1, s)
            policy = getPolicy(Q, self$epsilon)
            a = sampleActionFromPolicy(policy)
            
            while (envir$done == FALSE) {
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
              td.target = envir$reward + discount * (self$sigma * sarsa.target + 
                  (1 - self$sigma) * exp.sarsa.target)
              td.error = td.target - Q[a + 1]
              self$Q1[s + 1, a + 1] = self$Q1[s + 1, a + 1] + self$learning.rate * td.error
              
              s = s.n
              a = a.n
              Q = Q.n
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps

                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
                break
              }
            }
          }
        } else { # with eligibility
          self$runEpisode = function(envir, i) {
            self$E = matrix(0, nrow = nrow(self$Q1), ncol = envir$n.actions)
            envir$reset()
            s = self$preprocessState(envir$state)
            Q = self$predictQ(self$Q1, s)
            policy = getPolicy(Q, self$epsilon)
            a = sampleActionFromPolicy(policy)
            
            while (envir$done == FALSE) {
              envir$step(a)
              s.n = self$preprocessState(envir$state)
              Q.n = self$predictQ(self$Q1, s.n)
              policy = getPolicy(Q.n, self$epsilon)
              a.n = sampleActionFromPolicy(policy)
              
              self$E[s + 1, a + 1] = (1 - self$eligibility.type) * self$E[s + 1, a + 1] + 1
              
              if (target.policy == "greedy") {
                policy = getPolicy(Q.n, self$epsilon.target)
              }
              
              sarsa.target = Q.n[a.n + 1]
              exp.sarsa.target = sum(policy * Q.n)
              td.target = envir$reward + discount * (self$sigma * sarsa.target + 
                  (1 - self$sigma) * exp.sarsa.target)
              td.error = td.target - Q[a + 1]
              
              self$Q1 = self$Q1 + self$learning.rate * td.error * self$E
              
              self$E = discount * self$lambda * self$E *
                (self$sigma + policy[a.n + 1] * (1 - self$sigma))
              
              s = s.n
              a = a.n
              Q = Q.n
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
                
                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
                break
              }
            }
          }
        }
      }
      
      if (double.learning) {
        if (is.null(initial.value)) {
          self$Q2 = matrix(0, nrow = n.states, ncol = envir$n.actions)
        } else {
          self$Q2 = initial.value
        }
        
        if (!eligibility) {
          self$runEpisode = function(envir, i) {
            envir$reset()
            s = self$preprocessState(envir$state)
            Q = self$predictQ(self$Q1 + self$Q2, s)
            policy = getPolicy(Q, self$epsilon)
            a = sampleActionFromPolicy(policy)
            
            while (envir$done == FALSE) {
              envir$step(a)
              
              s.n = self$preprocessState(envir$state)
              Q.n = self$predictQ(self$Q1 + self$Q2, s.n)
              policy = getPolicy(Q.n, self$epsilon)
              a.n = sampleActionFromPolicy(policy)
              
              update.which.Q = sample(1:2, 1)
              if (update.which.Q == 1) {
                Q.A = self$predictQ(self$Q1, s.n)
                Q.B = self$predictQ(self$Q2, s.n)
              } else {
                Q.A = self$predictQ(self$Q2, s.n)
                Q.B = self$predictQ(self$Q1, s.n)
              }
              policy = getPolicy(Q.A, self$epsilon.target)  
              sarsa.target = Q.B[a.n + 1]
              exp.sarsa.target = sum(policy * Q.B)
              td.target = envir$reward + discount * (sigma * sarsa.target + 
                  (1 - sigma) * exp.sarsa.target)
              
              if (update.which.Q == 1) {
                Q = self$predictQ(self$Q1, s)
                td.error = td.target - Q[a + 1]
                self$Q1[s + 1, a + 1] = self$Q1[s + 1, a + 1] + self$learning.rate * td.error
              } else {
                Q = self$predictQ(self$Q2, s)
                td.error = td.target - Q[a + 1]
                self$Q2[s + 1, a + 1] = self$Q2[s + 1, a + 1] + self$learning.rate * td.error
              }
              
              s = s.n
              a = a.n
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
                
                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
                break
              }
            }
          }
        } else { # with eligibility
          self$runEpisode = function(envir, i) {
            self$E = matrix(0, nrow = nrow(self$Q1), ncol = envir$n.actions)
            envir$reset()
            s = self$preprocessState(envir$state)
            Q = self$predictQ(self$Q1 + self$Q2, s)
            policy = getPolicy(Q, self$epsilon)
            a = sampleActionFromPolicy(policy)
            
            while (envir$done == FALSE) {
              envir$step(a)
              
              s.n = self$preprocessState(envir$state)
              Q.n = self$predictQ(self$Q1 + self$Q2, s.n)
              policy = getPolicy(Q.n, self$epsilon)
              a.n = sampleActionFromPolicy(policy)
              
              self$E[s + 1, a + 1] = (1 - self$eligibility.type) * self$E[s + 1, a + 1] + 1
              
              update.which.Q = sample(1:2, 1)
              if (update.which.Q == 1) {
                Q.A = self$predictQ(self$Q1, s.n)
                Q.B = self$predictQ(self$Q2, s.n)
              } else {
                Q.A = self$predictQ(self$Q2, s.n)
                Q.B = self$predictQ(self$Q1, s.n)
              }
              policy = getPolicy(Q.A, self$epsilon.target)  
              sarsa.target = Q.B[a.n + 1]
              exp.sarsa.target = sum(policy * Q.B)
              td.target = envir$reward + discount * (sigma * sarsa.target + 
                  (1 - sigma) * exp.sarsa.target)
              
              if (update.which.Q == 1) {
                Q = self$predictQ(self$Q1, s)
                td.error = td.target - Q[a + 1]
                self$Q1 = self$Q1 + self$learning.rate * td.error * self$E
              } else {
                Q = self$predictQ(self$Q2, s)
                td.error = td.target - Q[a + 1]
                self$Q2 = self$Q2 + self$learning.rate * td.error * self$E
              }
              
              self$E = discount * self$lambda * self$E *
                (self$sigma + policy[a.n + 1] * (1 - self$sigma))
              
              s = s.n
              a = a.n
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
                
                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
                break
              }
            }
          }
        }
      }
      
      if (experience.replay) {
        if (!double.learning) {
          self$runEpisode = function(envir, i) {
            envir$reset()
            
            while (envir$done == FALSE) {
              s = self$preprocessState(envir$state)
              Q = self$predictQ(self$Q1, s)
              policy = getPolicy(Q, self$epsilon)
              a = sampleActionFromPolicy(policy)
              envir$step(a)
              
              self$replay.memory = self$add2ReplayMemory(envir, a)
              
              batch = self$sampleBatch()
              states = batch$state
              next.states = batch$next.state
              Q.old = self$Q1[states + 1, , drop = FALSE]
              Q.n = self$Q1[next.states + 1, , drop = FALSE]
              policy = t(apply(Q.n, 1, getPolicy, epsilon = self$epsilon))
              a.n = apply(policy, 1, sampleActionFromPolicy)
              sarsa.target = Q.n[matrix(c(seq_along(a.n), a.n + 1), ncol = 2)]
              if (target.policy == "greedy") {
                policy = t(apply(Q.n, 1, getPolicy, epsilon = self$epsilon.target))
              }
              exp.sarsa.target = rowSums(policy * Q.n)
              td.target = batch$reward + discount * (self$sigma * sarsa.target + 
                  (1 - self$sigma) * exp.sarsa.target)
              td.error = td.target - Q.old[matrix(c(seq_along(batch$action), batch$action + 1), ncol = 2)]
              
              self$Q1[matrix(c(states + 1, batch$action + 1), ncol = 2)] = 
                self$Q1[matrix(c(states + 1, batch$action + 1), ncol = 2)] + self$learning.rate * td.error
              
              self$priority = self$updatePriority(td.error, theta)
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
                
                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
                break
              }
            }
          }
        } else {
          self$runEpisode = function(envir, i) {
            envir$reset()
            
            while (envir$done == FALSE) {
              s = self$preprocessState(envir$state)
              Q = self$predictQ(self$Q1 + self$Q2, s)
              policy = getPolicy(Q, self$epsilon)
              a = sampleActionFromPolicy(policy)
              envir$step(a)
              
              self$replay.memory = self$add2ReplayMemory(envir, a)
              
              batch = self$sampleBatch()
              states = batch$state
              next.states = batch$next.state
             
              update.which.Q = sample(1:2, 1)
              if (update.which.Q == 1) {
                Q.A = self$Q1[next.states + 1, , drop = FALSE]
                Q.B = self$Q2[next.states + 1, , drop = FALSE]
              } else {
                Q.A = self$Q2[next.states + 1, , drop = FALSE]
                Q.B = self$Q1[next.states + 1, , drop = FALSE]
              }
              
              Q.new = (self$Q1 + self$Q2)[next.states + 1, , drop = FALSE]
              policy = t(apply(Q.new, 1, getPolicy, epsilon = self$epsilon))  
              a.n = apply(policy, 1, sampleActionFromPolicy)
              
              sarsa.target = Q.B[matrix(c(seq_along(a.n), a.n + 1), ncol = 2)]
              policy = t(apply(Q.A, 1, getPolicy, epsilon = self$epsilon.target))
              exp.sarsa.target = rowSums(policy * Q.B)
              td.target = batch$reward + discount * (sigma * sarsa.target + 
                  (1 - sigma) * exp.sarsa.target)
              
              if (update.which.Q == 1) {
                Q = self$Q1[states + 1, , drop = FALSE]
                td.error = td.target - Q[a + 1]
                self$Q1[matrix(c(states + 1, batch$action + 1), ncol = 2)] = 
                  self$Q1[matrix(c(states + 1, batch$action + 1), ncol = 2)] + self$learning.rate * td.error
              } else {
                Q = self$Q2[states + 1, , drop = FALSE]
                td.error = td.target - Q[a + 1]
                self$Q2[matrix(c(states + 1, batch$action + 1), ncol = 2)] = 
                  self$Q2[matrix(c(states + 1, batch$action + 1), ncol = 2)] + self$learning.rate * td.error
              }
              
              self$priority = self$updatePriority(td.error, theta)
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
                
                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
                break
              }
            }
          }
        }
      }
      
    }
    
    # ---- Linear Function Approximation
    if (fun.approx == "linear") {
      envir$reset()
      n.weights = length(preprocessState(envir$state))
      if (is.null(initial.value)) {
        self$Q1 = matrix(0, nrow = n.weights, ncol = envir$n.actions)
      } else {
        self$Q1 = initial.value
      }
      
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
            
            while (envir$done == FALSE) {
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
                
                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
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
            
            while (envir$done == FALSE) {
              envir$step(a)
              
              s.n = self$preprocessState(envir$state)
              Q.n = self$predictQ(self$Q1, s.n)
              policy = getPolicy(Q.n, self$epsilon)
              a.n = sampleActionFromPolicy(policy)
              
              self$E[, a + 1] = (1 - self$eligibility.type) * self$E[, a + 1] + s
              
              if (target.policy == "greedy") {
                policy = getPolicy(Q.n, self$epsilon.target)
              }
              
              sarsa.target = Q.n[a.n + 1]
              exp.sarsa.target = sum(policy * Q.n)
              td.target = envir$reward + discount * (sigma * sarsa.target + 
                  (1 - sigma) * exp.sarsa.target)
              td.error = td.target - Q[a + 1]
              
              self$Q1 = self$Q1 + self$learning.rate * td.error * self$E
              
              self$E = discount * self$lambda * self$E *
                (self$sigma + policy[a.n + 1] * (1 - self$sigma))
              
              s = s.n
              a = a.n
              Q = Q.n
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
                
                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
                break
              }
            }
          }
        }
      }
      
      if (double.learning) {
        if (is.null(initial.value)) {
          self$Q2 = matrix(0, nrow = n.weights, ncol = envir$n.actions)
        } else {
          self$Q2 = initial.value
        }
        
        if (!eligibility) {
          self$runEpisode = function(envir, i) {
            envir$reset()
            s = self$preprocessState(envir$state)
            Q = self$predictQ(self$Q1 + self$Q2, s)
            policy = getPolicy(Q, self$epsilon)
            a = sampleActionFromPolicy(policy)
            
            while (envir$done == FALSE) {
              envir$step(a)
              
              s.n = self$preprocessState(envir$state)
              Q.n = self$predictQ(self$Q1 + self$Q2, s.n)
              policy = getPolicy(Q.n, self$epsilon)
              a.n = sampleActionFromPolicy(policy)
              
              update.which.Q = sample(1:2, 1)
              if (update.which.Q == 1) {
                Q.A = self$predictQ(self$Q1, s.n)
                Q.B = self$predictQ(self$Q2, s.n)
              } else {
                Q.A = self$predictQ(self$Q2, s.n)
                Q.B = self$predictQ(self$Q1, s.n)
              }
              policy = getPolicy(Q.A, self$epsilon.target)  
              sarsa.target = Q.B[a.n + 1]
              exp.sarsa.target = sum(policy * Q.B)
              td.target = envir$reward + discount * (sigma * sarsa.target + 
                  (1 - sigma) * exp.sarsa.target)
              
              if (update.which.Q == 1) {
                Q = self$predictQ(self$Q1, s)
                td.error = td.target - Q[a + 1]
                self$Q1[, a + 1] = self$Q1[, a + 1] + self$learning.rate * td.error * s
              } else {
                Q = self$predictQ(self$Q2, s)
                td.error = td.target - Q[a + 1]
                self$Q2[, a + 1] = self$Q2[, a + 1] + self$learning.rate * td.error * s
              }
              
              s = s.n
              a = a.n
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
               
                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
                break
              }
            }
          }
        } else { # if eligibility
          self$runEpisode = function(envir, i) {
            self$E = matrix(0, nrow = nrow(self$Q1), ncol = envir$n.actions)
            envir$reset()
            s = self$preprocessState(envir$state)
            Q = self$predictQ(self$Q1 + self$Q2, s)
            policy = getPolicy(Q, self$epsilon)
            a = sampleActionFromPolicy(policy)
            
            while (envir$done == FALSE) {
              envir$step(a)
              
              s.n = self$preprocessState(envir$state)
              Q.n = self$predictQ(self$Q1 + self$Q2, s.n)
              policy = getPolicy(Q.n, self$epsilon)
              a.n = sampleActionFromPolicy(policy)
              
              self$E[, a + 1] = (1 - self$eligibility.type) * self$E[, a + 1] + s
              
              update.which.Q = sample(1:2, 1)
              if (update.which.Q == 1) {
                Q.A = self$predictQ(self$Q1, s.n)
                Q.B = self$predictQ(self$Q2, s.n)
              } else {
                Q.A = self$predictQ(self$Q2, s.n)
                Q.B = self$predictQ(self$Q1, s.n)
              }
              policy = getPolicy(Q.A, self$epsilon.target)  
              sarsa.target = Q.B[a.n + 1]
              exp.sarsa.target = sum(policy * Q.B)
              td.target = envir$reward + discount * (sigma * sarsa.target + 
                  (1 - sigma) * exp.sarsa.target)
              
              if (update.which.Q == 1) {
                Q = self$predictQ(self$Q1, s)
                td.error = td.target - Q[a + 1]
                self$Q1 = self$Q1 + self$learning.rate * td.error * self$E
              } else {
                Q = self$predictQ(self$Q2, s)
                td.error = td.target - Q[a + 1]
                self$Q2 = self$Q2 + self$learning.rate * td.error * self$E
              }
              
              self$E = discount * self$lambda * self$E *
                (self$sigma + policy[a.n + 1] * (1 - self$sigma))
              
              s = s.n
              a = a.n
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
                
                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
                break
              }
            }
          }
        }
      }
      
      if (experience.replay) {
        if (!double.learning) {
          self$runEpisode = function(envir, i) {
            envir$reset()
            
            while (envir$done == FALSE) {
              s = self$preprocessState(envir$state)
              Q = self$predictQ(self$Q1, s)
              policy = getPolicy(Q, self$epsilon)
              a = sampleActionFromPolicy(policy)
              envir$step(a)
              
              self$replay.memory = self$add2ReplayMemory(envir, a)
              
              batch = self$sampleBatch()
              
              Q.old = t(vapply(batch$state, self$predictQ, Q = self$Q1, 
                FUN.VALUE = numeric(envir$n.actions)))
              Q.n = t(vapply(batch$next.state, self$predictQ, Q = self$Q1, 
                FUN.VALUE = numeric(envir$n.actions)))
              
              policy = t(apply(Q.n, 1, getPolicy, epsilon = self$epsilon))
              a.n = apply(policy, 1, sampleActionFromPolicy)
              sarsa.target = Q.n[matrix(c(seq_along(a.n), a.n + 1), ncol = 2)]
              
              if (target.policy == "greedy") {
                policy = t(apply(Q.n, 1, getPolicy, epsilon = self$epsilon.target))
              }
              exp.sarsa.target = rowSums(policy * Q.n)
              td.target = batch$reward + discount * (self$sigma * sarsa.target + 
                  (1 - self$sigma) * exp.sarsa.target)
              td.error = td.target - Q.old[matrix(c(seq_along(batch$action), batch$action + 1), ncol = 2)]
              
              states = Reduce(rbind, batch$state)
              self$Q1[, batch$action + 1] = self$Q1[, batch$action + 1] + self$learning.rate * td.error * t(states)
              
              self$priority = self$updatePriority(td.error, theta)
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
                
                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
                break
              }
            }
          }
        } else {
          
        }
      }
    }
    
    # ---- Neural Network Function Approximation
    if (fun.approx == "neural.network") {
      self$predictQ = function(Q, state) {
        predict(Q, state)
      }
      
      keras::compile(model, loss = "mse", optimizer = keras::optimizer_sgd(lr = self$learning.rate))
      self$Q1 = model
      
      if (!experience.replay) {
        if (!eligibility) {
          self$runEpisode = function(envir, i) {
            envir$reset()
            s = self$preprocessState(envir$state)
            Q = self$predictQ(self$Q1, s)
            policy = getPolicy(Q, self$epsilon)
            a = sampleActionFromPolicy(policy)
            
            while (envir$done == FALSE) {
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
              td.target = envir$reward + discount * (self$sigma * sarsa.target + 
                  (1 - self$sigma) * exp.sarsa.target)
              x = s
              y = Q
              y[a + 1] = td.target
              keras::fit(self$Q1, x, y, verbose = 0, epochs = 1)
              
              s = s.n
              a = a.n
              Q = Q.n
              
              if (envir$done) {
                self$episode.steps[i] = envir$n.steps
                
                  message(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
                
                break
              }
            }
          }
        } 
      }
    }
  }
  
  # fixme: add updateparams
  # linear fun approx.: no list in replay memory
  # add episode return / reward sum
  # replace apply / vapply in linear fun approx. with matrix multiplication?
  # after initialization print out model and return this as a list at the end
)
)

# get epsilon-greedy policy with respect to Q 
getPolicy = function(Q, epsilon) {
  greedy.action = which.max(Q)
  n.actions = length(Q)
  policy = matrix(0, nrow = 1, ncol = n.actions)
  policy[, greedy.action] = 1 - epsilon
  policy = policy + epsilon / n.actions
  policy
}

# sample action from policy
sampleActionFromPolicy = function(policy) {
  action = sample(seq_along(policy), prob = policy, size = 1, replace = TRUE) - 1L
  action
}

initializeReplayMemory = function(envir, len, preprocessState) {
  replay.memory = vector("list", length = len)
  envir$reset()
  action = sample(envir$actions, 1)
  for (i in seq_len(len)) {
    envir$step(action)
    data = list(state = preprocessState(envir$previous.state), action = action,
      reward = envir$reward, next.state = preprocessState(envir$state))
    replay.memory[[i]] = data
    action = sample(envir$actions, 1)
    if (envir$done) {
      envir$reset()
    }
  }
  replay.memory
}
