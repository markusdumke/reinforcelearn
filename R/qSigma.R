#' Q(sigma) / Q-Learning / Sarsa
#' 
#' Value-based reinforcement learning control algorithms.
#' 
#' You must specify the reinforcement learning environment using the \code{envir} argument. 
#' It takes an \code{R6 class} created by \code{\link{makeEnvironment}} as input. 
#' See documentation there.
#' 
#' The algorithms can be used to find the optimal action value function using the principle 
#' of generalized policy iteration. They all learn online using temporal-difference learning. 
#' Q(sigma) subsumes the well known Q-Learning, Sarsa and Expected Sarsa algorithms as special cases.
#' The default call \code{qsigma()} is exactly equivalent to Sarsa(0). A weighted mean between sarsa
#' and expected sarsa updates can be used by varying the parameter \code{sigma}. 
#' When \code{target.policy == "e-greedy"} the policy used to compute the expected sarsa is the epsilon-greedy
#' policy used for action selection, when \code{target.policy == "greedy"} a greedy target policy will be 
#' used as in Q-Learning. See De Asis et al. (2017) for more details. 
#' 
#' The functions \code{qlearning} and \code{sarsa} are there for convenience. 
#' They call the \code{qSigma} function with a special set of parameters.
#' 
#' When \code{value.function == "table"} the action value function will be represented using a table, 
#' a linear combination of features and a neural network can also be used. For a neural network you 
#' need to pass on a keras model via the \code{model} argument. This way it is possible to 
#' construct a Deep Q-Network (DQN).
#' 
#' The raw state observation returned from the environment can be preprocessed using 
#' the \code{preprocessState} function. This function takes the state observation as input and 
#' returns a preprocessed state which can be directly used by the function approximator. 
#' This is especially important when using function approximation.
#' 
#' Experience replay can be used by specifying a prefilled replay memory through the 
#' \code{replay.memory} argument or by specifying the length of the replay memory, 
#' which will then be filled with random experience. Using the defaults experiences are sampled 
#' uniformly, a proportional prioritization proposed by Schaul et al. (2016) can also be used by 
#' varying the parameters \code{alpha} and \code{theta}.
#' 
#' Using eligibility traces the one-step algorithms can be extended to multi-step algorithms. The 
#' parameter \code{lambda} controls the tradeoff between one-step and multi-step methods. Three 
#' different kinds of eligibility traces are implemented, accumulating, replacing and dutch traces.
#' See Sutton and Barto (Book draft 2017, Chapter 12) for more details.
#' This only works if no experience replay is used, i.e. \code{replay.memory.size == 1} and for a 
#' tabular value function.
#' 
#' Double Learning can be used with all of the functions. 
#' Then two Q value functions are used, Q1 for action selection and 
#' Q2 for action evaluation. After a number of steps the weights of Q2 are replaced by the weights 
#' of Q1. See Hasselt et al. (2015) for details.
#' 
#' The hyperparameters \code{epsilon}, \code{theta}, \code{sigma}, \code{lambda} and
#' \code{learning.rate} can be changed over time. Therefore pass on functions that return a new 
#' value of the hyperparamter. These updates will be applied after each episode.
#' 
#' @param envir [\code{R6 class}] \cr 
#'   The reinforcement learning environment
#'   created by \code{\link{makeEnvironment}}.
#' @param value.function [\code{character(1)}] \cr 
#'   How to represent the value function? Currently \code{"table"} 
#'   and \code{"neural.network"} are supported.
#' @param sigma [\code{numeric(1) in [0, 1]}] \cr 
#'   Parameter of the Q(sigma) algorithm. It controls if the temporal-difference target 
#'   is equal to the sarsa target (for \code{sigma = 1}) or the expected sarsa target 
#'   (for \code{sigma = 0}). For intermediate values of \code{sigma} a weighted mean 
#'   between the two targets is used.
#' @param lambda [\code{numeric(1) in [0, 1]}] \cr 
#'   The \code{lambda} parameter combines different n-step returns using eligibility traces. 
#'   Only used if the replay memory is of size 1, i.e. no experience replay is used.
#' @param n.episodes [\code{integer(1)}] \cr 
#'   Number of episodes.
#' @param learning.rate [\code{R6 class}] \cr 
#'   Learning rate used for gradient descent.
#' @param epsilon [\code{numeric(1) in [0, 1]}] \cr 
#'   Ratio of random exploration in epsilon-greedy action selection.
#' @param updateEpsilon [\code{function]}] \cr 
#'   A function that updates epsilon. It takes two arguments, \code{epsilon} and the current number 
#'   of episodes which are finished, and returns the new \code{epsilon} value.
#' @param updateSigma [\code{function]}] \cr 
#'   A function that updates sigma. It takes two arguments, \code{sigma} and the current number 
#'   of episodes which are finished, and returns the new \code{sigma} value.
#' @param updateLambda [\code{function]}] \cr 
#'   A function that updates lambda. It takes two arguments, \code{lambda} and the current number 
#'   of episodes which are finished, and returns the new \code{lambda} value.
#' @param updateLearningRate [\code{function]}] \cr 
#'   A function that updates the learning rate. It takes two arguments, \code{learning.rate} 
#'   and the current number of episodes which are finished, and returns the new 
#'   \code{learning.rate} value.
#' @param updateAlpha [\code{function]}] \cr 
#'   A function that updates alpha. It takes two arguments, \code{alpha} and the current number 
#'   of episodes which are finished, and returns the new \code{alpha} value.
#' @param updateTheta [\code{function]}] \cr 
#'   A function that updates theta. It takes two arguments, \code{theta} and the current number 
#'   of episodes which are finished, and returns the new \code{theta} value.
#' @param initial.value [\code{numeric(1)}] \cr 
#'   Initial value for the value function. Only used with a tabular value function. 
#'   Set this to the maximal possible reward to encourage
#'   exploration (optimistic initialization).
#' @param discount [\code{numeric(1) in [0, 1]}] \cr 
#'   Discount factor.
#' @param target.policy [\code{character(1)}] \cr 
#'   Should the temporal-difference target be computed on-policy 
#'   using the epsilon-greedy behavior policy (\code{target.policy = "e-greedy"}) 
#'   or off-policy using a greedy policy in the 
#'   expected sarsa part of the update (\code{target.policy = "greedy"})?
#' @param double.learning [\code{logical(1)}] \cr 
#'   Should double learning be used?
#' @param replay.memory [\code{list}] \cr 
#'   Initial replay memory, which can be passed on. Make sure, it is the same size as 
#'   \code{replay.memory.size}. Each list element must be a list containing state, action, 
#'   reward and next.state. When the \code{replay.memory} argument is \code{NULL}, 
#'   the replay memory will be initially filled with random experiences.
#' @param replay.memory.size [\code{integer(1)}] \cr 
#'   Size of the replay memory.
#' @param batch.size [\code{integer(1)}] \cr 
#'   Batch size, how many experiences are sampled from the replay memory at each step? 
#'   Must be smaller than size of the replay memory!
#' @param alpha [\code{numeric(1) in [0, 1]}] \cr 
#'   If \code{alpha = 0} sampling from replay memory will be uniform, otherwise observations with
#'   high temporal-difference error will be proportionally prioritized.
#' @param theta [\code{numeric(1) in (0, 1]}] \cr 
#'   \code{theta} is a small positive constant that prevents the edge-case of transitions not being 
#'   revisited once their error is zero.
#' @param model [\code{keras model}] \cr 
#'   A neural network model specified using the \code{keras} package. 
#'   See Details for more information.
#' @param preprocessState [\code{function}] \cr 
#'   A function that takes the state observation returned from the environment as an input and 
#'   preprocesses this in a way the algorithm can work with it. See Details for more information.
#' @param update.target.after [\code{integer(1)}] \cr 
#'   When using double learning the target network / table will be updated after 
#'   \code{update.target.after} steps.
#' @param beta [\code{numeric(1)}] \cr 
#'   Type of eligibility trace, use \code{beta = 1} for replacing traces, 
#'   \code{beta = 0} for accumulating traces or intermediate values for a mixture between both. 
#'   Only used if the replay memory is of size 1, i.e. no experience replay is used. 
#'   Currently only supported for tabular value functions.
#' @rdname qSigma
#' @return [\code{list(2)}] \cr
#'   Returns the action value function or model parameters [\code{matrix}] and the 
#'   number of steps per episode [\code{numeric}].
#' @references De Asis et al. (2017): Multi-step Reinforcement Learning: A Unifying Algorithm
#' @references Hasselt et al. (2015): Deep Reinforcement Learning with Double Q-Learning
#' @references Mnih et al. (2013): Playing Atari with Deep Reinforcement Learning
#' @references Schaul et al. (2016): Prioritized Experience Replay
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @importFrom stats predict
#' @export
#' @examples 
#' grid = makeEnvironment(transitions = windy.gridworld$transitions,
#'   rewards = windy.gridworld$rewards,
#'   initial.state = 30L)
#'   
#' qSigma(grid, sigma = 0.5)
#' qlearning(grid)
#' sarsa(grid)
#' 
#' # Decay epsilon over time. Each 10 episodes epsilon will be halfed.
#' decayEpsilon = function(epsilon, i) {
#'   if (i %% 10 == 0) {
#'     epsilon = epsilon * 0.5
#'   }
#'   epsilon
#' }
#' 
#' qSigma(grid,  epsilon = 0.5, updateEpsilon = decayEpsilon)
#' 
#' # With linear function approximation
#' makeOneHot = function(state) {
#'   one.hot = matrix(rep(0L, 70L), nrow = 1)
#'   one.hot[1L, state + 1L] = 1L
#'   one.hot
#' }
#' 
#' # qSigma(grid, value.function = "linear", preprocessState = makeOneHot)
#' 
#' \dontrun{
#' # Neural network as function approximator
#' library(keras)
#' model = keras_model_sequential()
#' model %>% layer_dense(units = 4, activation = 'linear', input_shape = c(70))
#'   
#' qSigma(grid, value.function = "neural.network", model = model, preprocessState = makeOneHot)
#' }
#' 
qSigma = function(envir, value.function = "table", n.episodes = 100, sigma = 1, lambda = 0, 
  learning.rate = 0.1, epsilon = 0.1, discount = 1, target.policy = "e-greedy", 
  double.learning = FALSE, replay.memory = NULL, replay.memory.size = 1, 
  batch.size = 1, alpha = 0, theta = 0.01, beta = 0, 
  update.target.after = 1, preprocessState = NULL, model = NULL, 
  updateEpsilon = NULL, updateSigma = NULL, updateLambda = NULL, updateAlpha = NULL, 
  updateLearningRate = NULL, updateTheta = NULL, initial.value = 0) {
  
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$action.space == "Discrete")
  checkmate::assertNumber(discount, lower = 0, upper = 1)
  checkmate::assertNumber(learning.rate, lower = 0, upper = 1)
  checkmate::assertNumber(epsilon, lower = 0, upper = 1)
  checkmate::assertNumber(alpha, lower = 0, upper = 1)
  checkmate::assertNumber(theta, lower = 0, upper = 1)
  checkmate::assertNumber(initial.value)
  checkmate::assertNumber(lambda, lower = 0, upper = 1)
  checkmate::assertNumber(sigma, lower = 0, upper = 1)
  checkmate::assertNumber(beta, lower = 0, upper = 1)
  checkmate::assertInt(n.episodes, lower = 1)
  checkmate::assertInt(replay.memory.size, lower = 1)
  checkmate::assertInt(batch.size, lower = 1)
  checkmate::assertInt(update.target.after, lower = 1)
  if (replay.memory.size < batch.size) {
    stop("Batch size must be smaller than replay memory size!")
  }
  checkmate::assertChoice(value.function, c("table", "neural.network", "linear"))
  checkmate::assertChoice(target.policy, c("greedy", "e-greedy"))
  checkmate::assertFlag(double.learning)
  checkmate::assertList(replay.memory, types = "list", len = replay.memory.size, null.ok = TRUE)
  checkmate::assertList(replay.memory[[1]], len = 4, null.ok = TRUE,
    names = c("state", "action", "reward", "next.state"))
  checkmate::assertFunction(preprocessState, args = "state", ordered = TRUE, null.ok = TRUE)
  checkmate::assertFunction(updateEpsilon,  nargs = 2, null.ok = TRUE)
  checkmate::assertFunction(updateSigma,  nargs = 2, null.ok = TRUE)
  checkmate::assertFunction(updateLambda,  nargs = 2, null.ok = TRUE)
  checkmate::assertFunction(updateAlpha,  nargs = 2, null.ok = TRUE)
  checkmate::assertFunction(updateTheta,  nargs = 2, null.ok = TRUE)
  checkmate::assertFunction(updateLearningRate,  nargs = 2, null.ok = TRUE)
  
  agent = agent2$new(envir, value.function, n.episodes, sigma, lambda, 
    learning.rate, epsilon, discount, target.policy, 
    double.learning, replay.memory, replay.memory.size, 
    batch.size, alpha, theta, beta, 
    update.target.after, preprocessState, model, 
    updateEpsilon, updateSigma, updateLambda, updateAlpha, 
    updateLearningRate, updateTheta, initial.value)
  
  replay.steps = 0
  episode.steps = rep(0, n.episodes)
  
  for (i in seq_len(n.episodes)) {
    agent$resetEligibility()
    envir$reset()
    
    while(envir$done == FALSE) {
      replay.steps = replay.steps + 1
      if (replay.steps > replay.memory.size) {
        replay.steps = 1
      }
      agent$priority[replay.steps] = max(agent$priority)
      agent$interactWithEnvironment(envir)
      agent$add2ReplayMemory(index = replay.steps)
      
      agent$sampleBatch()
      
      agent$increaseEligibility() # make this work for function approximation
      agent$train()
      agent$reduceEligibility()
      
      if (double.learning & (envir$n.steps %% update.target.after == 0)) {
        agent$updateTargetModel()
      }
      
      if (envir$done) {
        episode.steps[i] = envir$n.steps
        print(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
        agent$updateParams()
        break
      }
    }
  }
  list(Q1 = agent$Q1, Q2 = agent$Q2, episode.steps = episode.steps)
}

# =================================================
# =================================================

agent2 = R6::R6Class("agent", 
  public = list(
    Q1 = NULL,
    Q2 = NULL,
    replay.memory = NULL,
    data = NULL,
    batch = NULL,
    priority = NULL,
    indices =  NULL,
    E = 1,
    policy = NULL,
    Q.predict = NULL,
    Q.target = NULL,
    td.target = NULL,
    td.error = NULL,
    
    action = NULL,
    next.action = NULL,
    
    epsilon = NULL,
    epsilon.target = NULL,
    sigma = NULL,
    lambda = NULL,
    theta = NULL,
    learning.rate = NULL,
    alpha = NULL,
    beta = NULL,
    discount = NULL,
    target.policy = NULL,
    batch.size = NULL,
    replay.memory.size = NULL,
    
    predictQ = NULL,
    resetEligibility = function() {},
    increaseEligibility = function() {},
    reduceEligibility = function() {},
    preprocessState = identity,
    # train = NULL,
    updateTargetModel = NULL,
    updateQ = NULL,
    getNextQ = NULL,
    
    updateEpsilon = function() {},
    updateSigma = function() {},
    updateLambda = function() {},
    updateAlpha = function() {},
    updateLearningRate = function() {},
    updateTheta = function() {},
    
    # initial weights, Q argument?
    # initialize table, neural.network, linear functions
    initialize = function(envir, value.function, n.episodes, sigma, lambda, 
      learning.rate, epsilon, discount, target.policy, 
      double.learning, replay.memory, replay.memory.size, 
      batch.size, alpha, theta, beta, 
      update.target.after, preprocessState, model, 
      updateEpsilon, updateSigma, updateLambda, updateAlpha, 
      updateLearningRate, updateTheta, initial.value) {
      
      self$epsilon = epsilon
      self$lambda = lambda
      self$sigma = sigma
      self$theta = theta
      self$alpha = alpha
      self$beta = beta
      self$learning.rate = learning.rate
      self$discount = discount
      self$target.policy = target.policy
      self$batch.size = batch.size
      self$replay.memory.size = replay.memory.size
      
      if (!is.null(updateEpsilon)) {
        self$updateEpsilon = updateEpsilon
      }
      if (!is.null(updateLambda)) {
        self$updateLambda = updateLambda
      }
      if (!is.null(updateAlpha)) {
        self$updateAlpha = updateAlpha
      }
      if (!is.null(updateTheta)) {
        self$updateTheta = updateTheta
      }
      if (!is.null(updateSigma)) {
        self$updateSigma = updateSigma
      }
      if (!is.null(updateLearningRate)) {
        self$updateLearningRate = updateLearningRate
      }
      
      if (value.function == "table") {
        self$initializeTable(initial.value)
      } else if (value.function == "neural.network") {
        self$initializeNeuralNetwork(model)
      } else if (value.function == "linear") {
        self$initializeLinear(envir, initial.value)
      }
      
      if (is.null(replay.memory)) {
        self$initializeReplayMemory(envir, replay.memory.size)
      } else {
        self$replay.memory = replay.memory
      }
      
      self$priority = rep(1, times = replay.memory.size)
      
      if (target.policy == "e-greedy") {
        self$epsilon.target = epsilon
      } else {
        self$epsilon.target = 0
      }
      
      if (double.learning) {
        self$getNextQ = function() {
          self$Q.predict = self$predictQ(self$Q1, unlist(self$batch$next.states))
          self$Q.target = self$predictQ(self$Q2, unlist(self$batch$next.states))
        }
      } else {
        self$getNextQ = function() {
          self$Q.predict = self$predictQ(self$Q1, unlist(self$batch$next.states)) # unlist does this work always?
          self$Q.target = self$Q.predict
        }
      }
    },
    
    initializeTable = function(initial.value) {
      self$Q1 = matrix(initial.value, nrow = envir$n.states, ncol = envir$n.actions)
      
      if (double.learning) {
        self$Q2 = matrix(initial.value, nrow = envir$n.states, ncol = envir$n.actions)
      }
      
      self$predictQ = function(Q, state) {
        Q[state + 1, , drop = FALSE]
      }
      
      self$updateTargetModel = function() {
        self$Q2 = self$Q1
      }
      
      if (self$replay.memory.size == 1) {
        self$updateQ = function() {
          self$Q1 = self$Q1 + self$learning.rate * self$td.error * self$E
        }
        self$resetEligibility = function() {
          self$E = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
        }
        
        self$increaseEligibility = function() {
          self$E[self$data$state + 1, self$data$action + 1] = (1 - self$beta) * 
            self$E[self$data$state + 1, self$data$action + 1] + 1
        }
        
        self$reduceEligibility = function(policy, next.action) {
          self$E = self$discount * self$lambda * self$E * 
            (self$sigma + self$policy[self$next.action + 1] * (1 - self$sigma))
        }
      } else {
        self$updateQ = function() {
          actions = self$batch$actions
          states = unlist(self$batch$states) # t(sapply(batch$states, preprocessState))
          self$Q1[matrix(c(states + 1, actions + 1), ncol = 2)] = self$Q1[matrix(c(states + 1, actions + 1), ncol = 2)] +
            self$learning.rate * self$td.error
        }
      }
    },
    
    initializeNeuralNetwork = function(model) {
      self$Q1 = model
      keras::compile(model, loss = 'mse', optimizer = keras::optimizer_sgd(lr = self$learning.rate))
      if (double.learning) {
        self$Q2 = model
      }
      
      self$predictQ = function(Q, state) {
        predict(Q, state)
      }
      
      # self$resetEligibility = function() {}
      # self$increaseEligibility = function() {}
      # self$reduceEligibility = function() {}
      
      self$updateTargetModel = function() {
        keras::set_weights(self$Q2, keras::get_weights(self$Q1))
      }
      
      self$updateQ = function() {
        states = self$batch$states
        actions = self$batch$actions
        Q.state = predictQ(self$Q1, states)
        y = Q.state
        y[matrix(c(seq_len(nrow(y)), actions + 1), ncol = 2)] = self$td.target
        keras::fit(self$Q1, states, y, verbose = 0)
      }
    },
    
    initializeLinear = function(envir, initial.value) {
      envir$reset()
      n.weights = length(preprocessState(envir$state))
      self$Q1 = rep(initial.value, n.weights)
      if (double.learning) {
        self$Q2 = rep(initial.value, n.weights)
      }
      
      self$predictQ = function(Q, state) {
        Q %*% state
      }
      
      # self$resetEligibility = function() {}
      # self$increaseEligibility = function() {}
      # self$reduceEligibility = function() {}
      
      self$updateTargetModel = function() {
        self$Q2 = self$Q1
      }
    },
    
    initializeReplayMemory = function(envir, len) {
      self$replay.memory = vector("list", length = len)
      envir$reset()
      action = sample(envir$actions, 1)
      for (i in seq_len(len)) {
        envir$step(action)
        self$replay.memory[[i]] = list(state = self$preprocessState(envir$previous.state), action = action,
          reward = envir$reward, next.state = self$preprocessState(envir$state))
        action = sample(envir$actions, 1)
        if (envir$done) {
          envir$reset()
        }
      }
    },
    
    interactWithEnvironment = function(envir) { # save preprocessed state in replay memory?
      s = self$preprocessState(envir$state)
      self$action = self$getAction(s)
      envir$step(self$action)
      self$data = list(state = s, action = self$action,
        reward = envir$reward, next.state = self$preprocessState(envir$state))
    },
    
    getAction = function(state) {
      Q.state = self$predictQ(self$Q1, state)
      self$returnPolicy(Q.state, self$epsilon)
      action = self$sampleAction()
      action
    },
    
    returnPolicy = function(Q, epsilon) {
      greedy.action = which.max(Q)
      n.actions = length(Q)
      self$policy = matrix(0, nrow = 1, ncol = n.actions)
      self$policy[, greedy.action] = 1 - epsilon
      self$policy = self$policy + epsilon / n.actions
    },
    
    sampleAction = function() {
      action = sample(seq_len(ncol(self$policy)), prob = self$policy, size = 1, replace = TRUE) - 1L
      action
    },
    
    add2ReplayMemory = function(index) {
      self$replay.memory[[index]] = self$data
    },
    
    getIndices = function() {
      probability = self$priority ^ self$alpha / sum(self$priority ^ self$alpha)
      self$indices = sample(seq_along(self$replay.memory), size = self$batch.size, prob = probability)
    },
    
    sampleBatch = function() {
      self$getIndices()
      batch = self$replay.memory[self$indices]
      states = lapply(batch, "[[", "state")
      next.states = lapply(batch, "[[", "next.state")
      actions = vapply(batch, "[[", "action", FUN.VALUE = integer(1))
      rewards = vapply(batch, "[[", "reward", FUN.VALUE = double(1))
      next.actions = vapply(next.states, self$getAction, FUN.VALUE = integer(1)) # put this in compute td error?
      self$next.action = next.actions
      self$batch = list(states = states, actions = actions, rewards = rewards, 
        next.states = next.states, next.actions = next.actions)
    }, 
    
    updateParams = function() {
      self$updateEpsilon()
      self$updateSigma()
      self$updateLambda()
      self$updateAlpha()
      self$updateTheta() # arguments episode_number
      if (self$target.policy == "e-greedy") {
        self$epsilon.target = self$epsilon
      }
    },
    
    updatePriority = function() {
      self$priority[self$indices] = abs(self$td.error) + self$theta
    },
    
    train = function(Q1, Q2) {
      self$computeTDTarget()
      self$computeTDError() # only necessary if linear, table
      self$updateQ()
      self$updatePriority()
    },
    
    computeTDTarget = function(Q1, Q2) {
      self$getNextQ()
      # sample next action here
      next.actions = self$batch$next.actions # another list?
      sarsa.target = self$Q.target[matrix(c(seq_len(nrow(self$Q.target)),
        next.actions + 1), ncol = 2)]
      policy = t(apply(self$Q.predict, 1, self$returnPolicy, epsilon = self$epsilon.target))
      exp.sarsa.target = rowSums(policy * self$Q.target)
      self$td.target = self$batch$rewards + self$discount * (self$sigma * sarsa.target +
          (1 - self$sigma) * exp.sarsa.target)
    },
    
    computeTDError = function() {
      states = unlist(self$batch$states) # t(sapply(batch$states, self$preprocessState))
      Q.state = self$predictQ(self$Q1, states)
      self$td.error = self$td.target - Q.state[matrix(c(seq_len(nrow(Q.state)), self$batch$actions + 1), ncol = 2)]
    }
  )
)

# Fixme: Error cannot use updateEpsilon: Error in self$updateEpsilon = updateEpsilon : 
# cannot change value of locked binding for 'updateEpsilon'
