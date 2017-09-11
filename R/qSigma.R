#' Q sigma / Q-Learning / Sarsa
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
#' When \code{on.policy == TRUE} the policy used to compute the expected sarsa is the epsilon-greedy
#' policy used for action selection, when \code{on.policy == FALSE} a greedy target policy will be 
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
#' @param eligibility [\code{character(1)}] \cr 
#'   Type of eligibility trace, could be \code{"replace"} for replacing traces, 
#'   \code{"accumulate"} for accumulating traces or \code{"dutch"} for dutch traces. 
#'   Only used if the replay memory is of size 1, i.e. no experience replay is used.
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
  batch.size = 1, alpha = 0, theta = 0.01, eligibility = "accumulate", 
  update.target.after = 1, preprocessState = NULL, model = NULL, 
  updateEpsilon = NULL, updateSigma = NULL, updateLambda = NULL, updateAlpha = NULL, 
  updateLearningRate = NULL, updateTheta = NULL, initial.value = 0) {
  
  # Fixme: implement this as class methods, add documentation and tests
  
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
  checkmate::assertInt(n.episodes, lower = 1)
  checkmate::assertInt(replay.memory.size, lower = 1)
  checkmate::assertInt(batch.size, lower = 1)
  checkmate::assertInt(update.target.after, lower = 1)
  if (replay.memory.size < batch.size) {
    stop("Batch size must be smaller than replay memory size!")
  }
  checkmate::assertChoice(value.function, c("table", "neural.network", "linear"))
  checkmate::assertChoice(eligibility, c("accumulate", "replace", "dutch"))
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
  # fixme neural network update learning.rate
  # Fixme: perform a check on model and preprocessState
  # Fixme: seed for python neural network?
  
  doNothing = function(x, y) {
    x
  }
  
  if (is.null(updateEpsilon)) {
    updateEpsilon = doNothing
  }
  if (is.null(updateLambda)) {
    updateLambda = doNothing
  }
  if (is.null(updateAlpha)) {
    updateAlpha = doNothing
  }
  if (is.null(updateTheta)) {
    updateTheta = doNothing
  }
  if (is.null(updateSigma)) {
    updateSigma = doNothing
  }
  if (is.null(updateLearningRate)) {
    updateLearningRate = doNothing
  }
  
  if (is.null(preprocessState)) {
    preprocessState = doNothing
  }
  
  if (target.policy == "e-greedy") {
    epsilon.target = epsilon
  } else {
    epsilon.target = 0
  }
  
  if (value.function == "table") {
    Q1 = matrix(initial.value, nrow = envir$n.states, ncol = envir$n.actions)
    if (double.learning) {
      Q2 = matrix(initial.value, nrow = envir$n.states, ncol = envir$n.actions)
    }
  } else if (value.function == "neural.network") {
    keras::compile(model, loss = 'mse', optimizer = keras::optimizer_sgd(lr = learning.rate))
    if (double.learning) {
      model_ = model
    }
  } else if (value.function == "linear") {
    envir$reset()
    n.weights = length(preprocessState(envir$state))
    Q1 = rep(initial.value, n.weights)
    if (double.learning) {
      Q2 = rep(initial.value, n.weights)
    }
  }
  
  if (is.null(replay.memory)) {
    replay.memory = initializeReplayMemory(envir, replay.memory.size)
  }
  priority = rep(1, times = replay.memory.size)
  replay.steps = 0
  episode.steps = rep(0, n.episodes)
  
  for (i in seq_len(n.episodes)) {
    if (value.function == "table") {
      E = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
    }
    envir$reset()
    
    while(envir$done == FALSE) {
      replay.steps = replay.steps + 1
      if (replay.steps > replay.memory.size) {
        replay.steps = 1
      }
      priority[replay.steps] = max(priority)
      data = interactWithEnvironment(value.function, envir, preprocessState, Q1, model, epsilon) # use Q1 + Q2
      if (value.function == "table" & replay.memory.size == 1) {
        E = increaseEligibility(eligibility, E, data$state, data$action, learning.rate)
      } else {
        E = 1
      }
      replay.memory = add2ReplayMemory(replay.memory, data, index = replay.steps)
      probability = priority ^ alpha / sum(priority ^ alpha)
      indexes = sample(seq_along(replay.memory), size = batch.size, prob = probability)
      batch = sampleBatch(replay.memory, batch.size, indexes)
      if (double.learning) {
        res = trainModel(value.function, batch, preprocessState, Q1, Q2, model, model_, 
          epsilon.target, discount, sigma, learning.rate, epsilon, batch.size,
          priority, theta, indexes, E, replay.memory.size)
      } else {
        res = trainModel(value.function, batch, preprocessState, Q1, Q1, model, model, 
          epsilon.target, discount, sigma, learning.rate, epsilon, batch.size,
          priority, theta, indexes, E, replay.memory.size)
      }
      Q1 = res$Q
      priority = res$priority
      if (value.function == "table" & replay.memory.size == 1) {
        E = reduceEligibility(E, lambda, discount)
      }
      if (double.learning & (envir$n.steps %% update.target.after == 0)) {
        Q2 = updateTargetModel(value.function, Q1, Q2, model, model_)
      }
      if (envir$done) {
        episode.steps[i] = envir$n.steps
        print(paste("Episode", i, "finished after", envir$n.steps, "time steps."))
        epsilon = updateEpsilon(epsilon, i)
        sigma = updateSigma(sigma, i)
        lambda = updateLambda(lambda, i)
        alpha = updateAlpha(alpha, i)
        theta = updateTheta(theta, i)
        learning.rate = updateLearningRate(learning.rate, i)
        if (target.policy == "e-greedy") {
          epsilon.target = epsilon
        }
        break
      }
    }
  }
  list(Q1 = Q1, episode.steps = episode.steps)
}

initializeReplayMemory = function(envir, len) {
  replay.memory = vector("list", length = len)
  envir$reset()
  action = sample(envir$actions, 1)
  for (i in seq_len(len)) {
    envir$step(action)
    replay.memory[[i]] = list(state = envir$previous.state, action = action,
      reward = envir$reward, next.state = envir$state) #, next.action = next.action)
    action = sample(envir$actions, 1)
    if (envir$done) {
      envir$reset()
    }
  }
  replay.memory
}

interactWithEnvironment = function(value.function, envir, preprocessState, Q, model, epsilon) {
  action = getAction(value.function, envir$state, preprocessState, Q, model, epsilon)
  envir$step(action)
  # next.action = getAction(envir$state, Q)
  list(state = envir$previous.state, action = action,
    reward = envir$reward, next.state = envir$state) #, next.action = next.action)
}

getAction = function(value.function, state, preprocessState, Q, model, epsilon) {
  state.prep = preprocessState(state)
  Q.state = predictQ(value.function, state.prep, Q, model)
  policy = returnPolicy(Q.state, epsilon)
  action = sampleAction(policy, 1)
  action
}

predictQ = function(value.function, state, Q, model) {
  if (value.function == "table") {
    Q[state + 1, , drop = FALSE]
  } else if (value.function == "neural.network") {
    predict(model, state)
  } else { # linear
    Q %*% state
  }
}

returnPolicy = function(Q, epsilon) {
  greedy.action = which.max(Q)
  n.actions = length(Q)
  policy = matrix(0, nrow = 1, ncol = n.actions)
  policy[, greedy.action] = 1 - epsilon
  policy = policy + epsilon / n.actions
  policy
}

sampleAction = function(policy, size) {
  action = sample(seq_len(ncol(policy)), prob = policy, size = size, replace = TRUE) - 1L
  action
}

increaseEligibility = function(eligibility, E, state, action, learning.rate) {
  if (eligibility == "accumulate") {
    E[state + 1, action + 1] = E[state + 1, action + 1] + 1
  } else if (eligibility == "replace") {
    E[state + 1, ] = 0
    E[state + 1, action + 1] = 1
  } else {
    E[state + 1, ] = 0
    E[state + 1, action + 1] = (1 - learning.rate) * E[state + 1, action + 1] + 1
  }
  E
}

reduceEligibility = function(E, lambda, discount) {
  E = lambda * discount * E
  E
}

add2ReplayMemory = function(replay.memory, data, index) {
  replay.memory[[index]] = data
  replay.memory
}

sampleBatch = function(replay.memory, batch.size, indexes) {
  batch = replay.memory[indexes]
  states = lapply(batch, "[[", "state")
  next.states = lapply(batch, "[[", "next.state")
  actions = vapply(batch, "[[", "action", FUN.VALUE = double(1))
  rewards = vapply(batch, "[[", "reward", FUN.VALUE = double(1))
  # next.actions = vapply(batch, "[[", "next.action", FUN.VALUE = double(1))
  list(states = states, actions = actions,
    rewards = rewards, next.states = next.states) #, next.actions = next.actions)
}

trainModel = function(value.function, batch, preprocessState, Q1, Q2, model, model_, 
  epsilon.target, discount, sigma, learning.rate, epsilon, batch.size, 
  priority, theta, indexes, E, replay.memory.size) {
  td.target = computeTDTarget(value.function, batch, preprocessState, Q1, Q2, model, model_, 
    epsilon.target, discount, sigma, epsilon, batch.size)
  td.error = computeTDError(value.function, batch, td.target, preprocessState, Q1, model)
  Q = updateQ(value.function, preprocessState, Q1, model, batch, learning.rate, 
    td.target, td.error, E, replay.memory.size)
  priority = updatePriority(priority, td.error, theta, indexes)
  list(Q = Q, priority = priority)
}

computeTDTarget = function(value.function, batch, preprocessState, Q1, Q2, 
  model, model_, epsilon.target, discount, sigma, epsilon, batch.size) {
  next.states = t(sapply(batch$next.states, preprocessState))
  Q.next.state = predictQ(value.function, next.states, Q1, model)
  Q.next.state.target = predictQ(value.function, next.states, Q2, model_)
  # next.actions = batch$next.actions
  policy = t(apply(Q.next.state, 1, returnPolicy, epsilon = epsilon))
  actions = rep(NA, batch.size)
  for (i in seq_len(batch.size)) {
    actions[i] = sampleAction(policy[i, , drop = FALSE], size = 1)
  }
  sarsa.target = Q.next.state.target[matrix(c(seq_len(nrow(Q.next.state.target)),
    actions + 1), ncol = 2)]
  policy = t(apply(Q.next.state, 1, returnPolicy, epsilon = epsilon.target))
  exp.sarsa.target = rowSums(policy * Q.next.state.target)
  td.target = batch$rewards + discount * (sigma * sarsa.target +
      (1 - sigma) * exp.sarsa.target)
  td.target
}

computeTDError = function(value.function, batch, td.target, preprocessState, Q1, model) {
  states = t(sapply(batch$states, preprocessState))
  Q.state = predictQ(value.function, states, Q1, model)
  td.error = td.target - Q.state[matrix(c(seq_len(nrow(Q.state)), batch$actions + 1), ncol = 2)]
  td.error
}

updateQ = function(value.function, preprocessState, Q1, model, batch, learning.rate, td.target, td.error, E, replay.memory.size) {
  states = t(sapply(batch$states, preprocessState))
  Q.state = predictQ(value.function, states, Q1, model)
  actions = batch$actions
  if (value.function == "table") {
    if (replay.memory.size == 1) {
      Q1 = Q1 + learning.rate * td.error * E
    } else {
      Q1[matrix(c(states + 1, actions + 1), ncol = 2)] = Q1[matrix(c(states + 1, actions + 1), ncol = 2)] +
        learning.rate * td.error * E
    }
    return(Q1)
  } else if (value.function == "neural.network") {
    y = Q.state
    y[matrix(c(seq_len(nrow(y)), actions + 1), ncol = 2)] = td.target
    keras::fit(model, states, y, verbose = 0)
  }
}

updateTargetModel = function(value.function, Q1, Q2, model, model_) {
  if (value.function == "table") {
    return(Q1)
  } else if (value.function == "neural.network") {
    keras::set_weights(model_, keras::get_weights(model))
  }
}

updatePriority = function(priority, td.error, theta, indexes) {
  priority[indexes] = abs(td.error) + theta
  priority
}

# Argmax (ties broken randomly)
# x numeric matrix or numeric vector
argmax = function(x) {
  nnet::which.is.max(x)
}

# decayParam = function(param, time, when) {
#   if (time %% when == 0) {
#     param = param * decay.factor
#   }
#   param
# }
