#' Q-Learning with Function Approximation
#'
#' Q-Learning algorithm with Experience Replay and
#' Frozen Target Network and Double Q-Learning.
#'
#' To use experience replay you can
#' either specify an initial replay memory filled with experience
#' and provide the size of the replay memory. If you do not
#' specify a replay memory this will be initially filled with random
#' experience.
#' Double Q-Learning works right now only if `frozen.target == TRUE`.
#'
#' @inheritParams params
#'
#' @return list with weights and number of steps
#' @export
#' @seealso [qlearning]
#' @references Hasselt et al. (2015): Deep Reinforcement Learning with Double Q-Learning
#' @references Mnih et al. (2013): Playing Atari with Deep Reinforcement Learning
#' @references Schaul et al. (2016): Prioritized Experience Replay
#' @examples
#' # define the environment
#' grid = WindyGridworld$new()
#' WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array,
#'   reward.matrix = grid$reward.matrix,
#'   initial.state = 30L)
#'
#' # Build the DQN
#' # define a tensorflow graph for the function approximator (here a neural network)
#' library(tensorflow)
#' tf$reset_default_graph()
#' inputs = tf$placeholder(tf$float32, shape(NULL, WindyGridworld1$n.states))
#' weights = tf$Variable(tf$random_uniform(shape(WindyGridworld1$n.states,
#'   WindyGridworld1$n.actions), 0, 0.01, seed = 1))
#' Q = tf$matmul(inputs, weights)
#' nextQ = tf$placeholder(tf$float32, shape(NULL, WindyGridworld1$n.actions))
#' loss = tf$reduce_sum(tf$square(nextQ - Q))
#' optimizer = tf$train$GradientDescentOptimizer(learning_rate = 0.1)
#' trainModel = optimizer$minimize(loss)
#'
#' # initialize the session and the weights
#' sess = tf$Session()
#' sess$run(tf$global_variables_initializer())
#'
#' # takes the state and returns a one-hot vector
#' preprocessState = function(state_) {
#'   one_hot = matrix(0L, nrow = length(state_), ncol = WindyGridworld1$n.states)
#'   one_hot[cbind(seq_along(state_), state_)] = 1L
#'   one_hot
#' }
#' # predict returns vector of q values for a given state
#' predict = function(inputs_) {
#'   sess$run(Q, feed_dict = dict(inputs = inputs_))
#' }
#' # train model, update weights, e.g. gradient descent: this is supervised learning
#' train = function(inputs_, outputs_, predictions_ = NULL) {
#'   sess$run(tuple(trainModel, weights),
#'     feed_dict = dict(inputs = inputs_, nextQ = outputs_))
#' }
#'
#' # Q-Learning
#' res = qlearning2(WindyGridworld1, n.episodes = 100L,
#'   preprocessState, predict, train, seed = 2L)
#'
qlearning2 <- function(envir, n.episodes = 10L, preprocessState = NULL,
  predict = NULL, train = NULL, predict2 = NULL, copy = NULL,
  double.qlearning = FALSE, experience.replay = FALSE, replay.memory = NULL,
  replay.memory.size = 1000L, initial.replay.memory.size = 1000L,
  batch.size = 32L, alpha = 0, theta = 0.01, 
  frozen.target = FALSE, update.target.after = 100L,
  epsilon = 0.1, epsilon.decay = 0.5, epsilon.decay.after = 100L,
  discount.factor = 1, seed = NULL, ...) {
  
  # input checking
  if (!is.null(seed)) { set.seed(seed) } # set random seed
  if (double.qlearning == TRUE) {
    if(frozen.target == FALSE) {
      stop("Double Q-Learning only works when frozen.target == TRUE")
    }
  }
  if (replay.memory.size < length(replay.memory)) {
    stop("replay.memory.size must be at least as large as the length of provided replay.memory.")
  }
  
  # experience replay
  # fill initial replay memory randomly if no experience is supplied
  if (experience.replay == TRUE) {
    if (is.null(replay.memory)) {
      replay.memory = vector("list", length = replay.memory.size) # fixed size
      initial.replay.memory.size = replay.memory.size
      envir$reset()
      for (i in seq_len(initial.replay.memory.size)) {
        state = envir$state
        action = sample(envir$actions, 1L)
        envir$step(action)
        replay.memory[[i]] <- list(action = action, reward = envir$reward,
          state = state, next.state = envir$state)
        if (envir$episode.over == TRUE) {
          envir$reset()
        }
      }
      print("Replay memory randomly initialized!")
    } else {
      initial.replay.memory.size = length(replay.memory)
      replay.memory = append(replay.memory,
        vector("list", replay.memory.size - initial.replay.memory.size))
    }
  } else {
    replay.memory = vector("list", length = 1L)
    replay.memory.size = 1L
    initial.replay.memory.size = 1L
    batch.size = 1L
  }
  # initialize priority uniformly
  priority = rep(c(1, 0), times = c(initial.replay.memory.size, 
    replay.memory.size - initial.replay.memory.size))
    
  # statistics about learning behaviour: steps per episode
  steps.per.episode = rep(0L, n.episodes)
  steps = 0L # counts total number of steps
  replay.steps = 0L
  
  for (i in seq_len(n.episodes)) {
    envir$reset()
    state = envir$state # initial state
    steps.this.episode = 0L # counts number of steps in current episode
    
    while (envir$episode.over == FALSE) {
      steps.this.episode = steps.this.episode + 1L # increase step counter
      steps = steps + 1L
      # print(steps)
      replay.steps = replay.steps + 1L
      
      features.state_ = preprocessState(state, ...)
      Q.state.online = predict(features.state_, ...)
      action = sample_epsilon_greedy_action(Q.state.online, epsilon)
      envir$step(action)
      next.state = envir$state
      
      # if replay memory is full, replace oldest experience
      if (experience.replay == TRUE) {
        if (initial.replay.memory.size + steps <= replay.memory.size) {
          replay.steps = initial.replay.memory.size + steps
        }
        if (initial.replay.memory.size + steps > replay.memory.size) {
          replay.steps = 1L
        }
        replay.memory[[replay.steps]] = list(action = action, reward = envir$reward,
          state = state, next.state = next.state)
        priority[replay.steps] = max(priority)
      } else { # if experience.replay == FALSE we will just store the latest transition
        replay.memory[[1L]] = list(action = action, reward = envir$reward,
          state = state, next.state = next.state)
        priority = 1
      }
      
      # sample a batch of transitions s, a, r, s' from replay.memory
      # sample probabilities prioritized by td error
      probability = priority ^ alpha / sum(priority ^ alpha)
      indexes = sample(seq_len(min(replay.memory.size,
        initial.replay.memory.size + steps)), batch.size, prob = probability)
      batch = replay.memory[indexes]
      batch.states = lapply(batch, "[[", "state")
      batch.next.states = lapply(batch, "[[", "next.state")
      batch.actions = vapply(batch, "[[", "action", FUN.VALUE = double(1))
      batch.rewards = vapply(batch, "[[", "reward", FUN.VALUE = double(1))
      features.batch.states = Reduce(rbind, lapply(batch.states, preprocessState))
      features.batch.next.states = Reduce(rbind, lapply(batch.next.states, preprocessState))
      
      # our current estimate of the q values
      Q.state = predict(features.batch.states, ...)
      
      # compute target values
      # for double qlearning action selection and evaluation are decoupled
      if (frozen.target == FALSE) {
        Q.next.state = predict(features.batch.next.states, ...)
        td.target =  batch.rewards + discount.factor * apply(Q.next.state, 1, max)
        # td.target =  batch.rewards + discount.factor * Q.next.state[argmax(Q.next.state)] # equivalent to the above
      } else {
        if (double.qlearning == FALSE) {
          Q.next.state = predict2(features.batch.next.states, ...)
          td.target =  batch.rewards + discount.factor * apply(Q.next.state, 1, max)
        } else {
          Q.next.state = predict2(features.batch.next.states, ...)
          Q.next.state.online = predict(features.batch.next.states, ...)
          td.target =  batch.rewards + discount.factor * Q.next.state[argmax(Q.next.state.online)]
        }
      }
      
      target.Q = Q.state
      target.Q[cbind(seq_along(batch.actions), batch.actions + 1)] = td.target
      weights = train(features.batch.states, target.Q, Q.state, ...) # train on minibatch
      
      td.error = td.target - Q.state[action + 1] # Q.state of online network
      priority[indexes] = abs(td.error) + theta
      
      # update target network: get all weights and copy them to target network
      if (frozen.target == TRUE) {
        if (steps %% update.target.after == 0L) {
          # tf$trainable$variables()
          copy()
          # print("Copied model parameters to target network.")
        }
      }
      
      state = next.state
      
      if (envir$episode.over) {
        if (i %% epsilon.decay.after == 0L) {
          epsilon = epsilon * epsilon.decay
        }
        steps.per.episode[i] = steps.this.episode
        print(paste("Episode", i, "finished after", steps.this.episode, "time steps."))
        break
      }
    }
  }
  
  # close python window for gym environment
  envir$close()
  
  list(weights = weights, steps.per.episode = steps.per.episode)
}

################################################
################################################
################################################
# # no experience replay, no frozen targets
# source("C:/Users/M/Desktop/reinforcelearn/R/build_model.R")
# res1 = qlearning2(WindyGridworld1, n.episodes = 300L,
#   preprocessState, predict, train, seed = 2L)
# 
# # no experience replay, no frozen targets
# source("C:/Users/M/Desktop/reinforcelearn/R/build_model.R")
# res8 = qlearning2(WindyGridworld1, n.episodes = 1000L,
#   preprocessState, predict, train, experience.replay = TRUE,
#   replay.memory.size = 10000L, initial.replay.memory.size = 10000L,
#   alpha = 10, seed = 2L)
# # 
# # no experience replay, with frozen targets, with double qlearning
# source("C:/Users/M/Desktop/reinforcelearn/R/build_model.R")
# res2 = qlearning2(WindyGridworld1, n.episodes = 300L,
#   preprocessState, predict, train, predict2, copy, double.qlearning = TRUE,
#   frozen.target = TRUE, update.target.after = 100L, seed = 2L)
# 
# # with experience replay, with frozen targets, with double qlearning
# source("C:/Users/M/Desktop/reinforcelearn/R/build_model.R")
# res3 = qlearning2(WindyGridworld1, n.episodes = 300L,
#   preprocessState, predict, train, predict2, copy, double.qlearning = TRUE,
#   experience.replay = TRUE, replay.memory.size = 10000L,
#   initial.replay.memory.size = 10000L,
#   frozen.target = TRUE, update.target.after = 100L, seed = 2L)
# 
# # with experience replay, no frozen targets
# source("C:/Users/M/Desktop/reinforcelearn/R/build_model.R")
# res4 = qlearning2(WindyGridworld1, n.episodes = 300L,
#   preprocessState, predict, train,
#   experience.replay = TRUE, replay.memory.size = 10000L,
#   initial.replay.memory.size = 10000L, seed = 5L)
# 
# # no experience replay, with frozen targets
# source("C:/Users/M/Desktop/reinforcelearn/R/build_model.R")
# res5 = qlearning2(WindyGridworld1, n.episodes = 300L,
#   preprocessState, predict, train, predict2, copy,
#   frozen.target = TRUE, update.target.after = 100L, seed = 2L)
# 
# # with experience replay, with frozen targets
# source("C:/Users/M/Desktop/reinforcelearn/R/build_model.R")
# res6 = qlearning2(WindyGridworld1, n.episodes = 300L,
#   preprocessState, predict, train, predict2, copy,
#   experience.replay = TRUE, replay.memory.size = 10000L,
#   initial.replay.memory.size = 10000L,
#   frozen.target = TRUE, update.target.after = 100L, seed = 2L)
# 
# # with experience replay passed on to function
# # pass initial random experience to the function
# replay.memory.size = 10000L # steps
# replay.memory2 = vector("list", length = replay.memory.size)
# # fill this initially with experience generated by random policy
# WindyGridworld1$reset()
# for (i in 1:replay.memory.size) {
#   state = WindyGridworld1$state
#   action = sample(0:3, 1)
#   WindyGridworld1$step(action)
#   replay.memory2[[i]] <- list(action = action, reward = WindyGridworld1$reward,
#     state = state, next.state = WindyGridworld1$state)
#   if (WindyGridworld1$episode.over == TRUE) {
#     print(i)
#     WindyGridworld1$reset()
#   }
# }
# 
# source("C:/Users/M/Desktop/reinforcelearn/R/build_model.R")
# res7 = qlearning2(WindyGridworld1, n.episodes = 300L,
#   preprocessState, predict, train,
#   experience.replay = TRUE, replay.memory = replay.memory2,
#   replay.memory.size = 10000L, seed = 2L)
