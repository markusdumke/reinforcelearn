#' Q-Learning with Function Approximation
#' 
#' You can use Q-Learning with Experience Replay. Therefore you can 
#' either specify an initial replay memory filled with experience
#' and provide the size of the replay memory. If you do not
#' specify a replay memory this will be initially filled with random 
#' experience.
#'
#' @inheritParams params
#'
#' @return list with weights
#' @export
#' @seealso [qlearning]
#'
#'
qlearning2 <- function(envir, n.episodes = 10L,
  preprocessState = NULL, predict = NULL, train = NULL, # predict2 = NULL, 
  double.qlearning = FALSE, experience.replay = FALSE, replay.memory = NULL,
  replay.memory.size = 1000L, batch.size = 32L, frozen.target = FALSE, update.target.after = 100L,
  epsilon = 0.1, epsilon.decay = 0.5, epsilon.decay.after = 100L,
  discount.factor = 1, seed = NULL, ...) {
  
  if (!is.null(seed)) { set.seed(seed) } # set random seed
  # if (replay.memory.size < length(replay.memory)) {
  #   stop("replay.memory.size must be at least as large as the lebntgh of a provided replay.memory.")
  # }
  
  # experience replay
  # fill initial replay memory randomly if non supplied
  if (experience.replay == TRUE) {
    if (is.null(replay.memory)) {
      replay.memory = vector("list", length = replay.memory.size) # fixed size
      initial.replay.memory.size = replay.memory.size
      envir$reset()
      for (i in seq_len(replay.memory.size)) {
        state = envir$state
        action = sample(envir$actions, 1L)
        envir$step(action)
        replay.memory[[i]] <- list(action = action, reward = envir$reward,
          state = state, next.state = envir$state)
        if (envir$episode.over == TRUE) {
          envir$reset()
        }
      }
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
  
  # statistics about learning behaviour: steps per episode
  steps.per.episode = rep(0L, n.episodes)
  steps = 0L # counts total number of steps
  replay.steps = 0
  
  for (i in seq_len(n.episodes)) {
    envir$reset()
    state = envir$state # initial state
    steps.this.episode = 0L # counts number of steps in current episode
    
    while (envir$episode.over == FALSE) {
      steps.this.episode = steps.this.episode + 1L # increase step counter
      steps = steps + 1L
      replay.steps = replay.steps + 1
      
      features.state_ = preprocessState(state, ...)
      Q.state = predict(features.state_, ...)
      action = sample_epsilon_greedy_action(Q.state, epsilon)
      envir$step(action)
      next.state = envir$state
      
      # if replay memory is full, replace oldest experience
      if (experience.replay == TRUE) {
        if (initial.replay.memory.size + steps <= replay.memory.size) {
          replay.steps = initial.replay.memory.size + steps
        }
        if (initial.replay.memory.size + steps > replay.memory.size) {
          replay.steps = 1
        }
        
        replay.memory[[replay.steps]] = list(action = action, reward = envir$reward,
          state = state, next.state = next.state)
      } else { # if experience.replay == FALSE we will just store the latest transition
        replay.memory[[1]] = list(action = action, reward = envir$reward,
          state = state, next.state = next.state)
      }
      
      # sample a batch of transitions s, a, r, s' from replay.memory
      indexes = sample(seq_len(min(replay.memory.size,
        initial.replay.memory.size + steps)), batch.size)
      batch = replay.memory[indexes]
      
      batch.states = lapply(batch, "[[", "state")
      batch.next.states = lapply(batch, "[[", "next.state")
      batch.actions = vapply(batch, "[[", "action", FUN.VALUE = double(1)) # sapply(batch, "[[", "action")
      batch.rewards = vapply(batch, "[[", "reward", FUN.VALUE = double(1)) # sapply(batch, "[[", "reward")
      
      features.state = Reduce(rbind, lapply(batch.states, preprocessState))
      features.next.state = Reduce(rbind, lapply(batch.next.states, preprocessState))
      
      # frozen target, update weights only occasionaly
      # if (frozen.target == TRUE) {
      #  Q.state = predict2(features.state, ...)
      #  Q.next.state = predict2(features.next.state, ...) # use frozen target network
      # } else {
      Q.state = predict(features.state, ...)
      Q.next.state = predict(features.next.state, ...)
      # }
      
      td.target =  batch.rewards + discount.factor * apply(Q.next.state, 1, max) # max over rows
      target.Q = Q.state
      target.Q[cbind(seq_along(batch.actions), batch.actions + 1)] = td.target
      
      weights = train(features.state, target.Q, Q.state, ...) # train on minibatch
      
      # # update target network: get all weights and copy them to target network
      # if (frozen.target == TRUE) {
      #   if (steps %% update.target.after == 0) {
      #     # e1_params = [t for t in tf.trainable_variables() if t.name.startswith(estimator1.scope)]
      #     # e1_params = sorted(e1_params, key=lambda v: v.name)
      #     # e2_params = [t for t in tf.trainable_variables() if t.name.startswith(estimator2.scope)]
      #     # e2_params = sorted(e2_params, key=lambda v: v.name)
      #     # update_ops = []
      #     # for e1_v, e2_v in zip(e1_params, e2_params):
      #     #   op = e2_v.assign(e1_v)
      #     # update_ops.append(op)
      #     # sess.run(update_ops)
      #     #
      #     # self.model_.set_weights(self.model.get_weights())
      #     tf$assign(weights2, weights[[2]])
      #     # print("Copied model parameters to target network.")
      #   }
      # }
      
      state = next.state
      
      if (envir$episode.over) {
        if (i %% epsilon.decay.after == 0) {
          epsilon = epsilon * epsilon.decay
        }
        steps.per.episode[i] = steps.this.episode
        print(paste("Episode", i, "finished after", steps.this.episode, "time steps."))
        break
      }
    }
  }
  # # env_monitor_close(envir$client, envir$instance_id)
  list(weights = weights, steps.per.episode = steps.per.episode)
}




# 
# 
# # define the environment
# grid = WindyGridworld$new()
# WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array,
#   reward.matrix = grid$reward.matrix,
#   initial.state = 30L)
# 
# # define a tensorflow graph for the function approximator (here a neural network)
# library(tensorflow)
# tf$reset_default_graph()
# batch.size = 32L
# inputs = tf$placeholder(tf$float32, shape(NULL, WindyGridworld1$n.states))
# weights = tf$Variable(tf$random_uniform(shape(WindyGridworld1$n.states,
#   WindyGridworld1$n.actions), 0, 0.01))
# Q = tf$matmul(inputs, weights)
# nextQ = tf$placeholder(tf$float32, shape(NULL, WindyGridworld1$n.actions))
# loss = tf$reduce_sum(tf$square(nextQ - Q))
# optimizer = tf$train$GradientDescentOptimizer(learning_rate = 0.1)
# trainModel = optimizer$minimize(loss)
# 
# # initialize the session and the weights
# sess = tf$Session()
# sess$run(tf$global_variables_initializer())
# 
# # takes the state and returns a one-hot vector
# preprocessState = function(state_) {
#   one_hot = matrix(0L, nrow = length(state_), ncol = WindyGridworld1$n.states)
#   one_hot[cbind(seq_along(state_), state_)] = 1L
#   one_hot
# }
# 
# # predict returns vector of q values for a given state
# predict = function(inputs_) {
#   sess$run(Q, feed_dict = dict(inputs = inputs_))
# }
# 
# # train model, update weights, e.g. gradient descent: this is supervised learning
# train = function(inputs_, outputs_, predictions_ = NULL) {
#   sess$run(tuple(trainModel, weights),
#     feed_dict = dict(inputs = inputs_, nextQ = outputs_))
# }
# 
# # # no experience replay
# res = qlearning2(WindyGridworld1, n.episodes = 100L,
#   preprocessState, predict, train,
#   double.qlearning = FALSE, experience.replay = FALSE, replay.memory = NULL,
#   replay.memory.size = 10000L, batch.size = 32L, update.target.after = 100L,
#   epsilon = 0.1, epsilon.decay = 0.5, epsilon.decay.after = 100L,
#   discount.factor = 1, seed = NULL)
# 
# # experience replay, this is much better!
# res = qlearning2(WindyGridworld1, n.episodes = 100L,
#   preprocessState, predict, train,
#   double.qlearning = FALSE, experience.replay = TRUE, replay.memory = NULL,
#   replay.memory.size = 10000L, batch.size = 32L, update.target.after = 100L,
#   epsilon = 0.1, epsilon.decay = 0.5, epsilon.decay.after = 100L,
#   discount.factor = 1, seed = NULL)
# 
# # pass initial random experience to the function
# replay.memory.size = 10000 # steps
# replay.memory2 = vector("list", length = replay.memory.size)
# 
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
# # debug(qlearning2)
# # undebug(qlearning2)
# #
# res = qlearning2(WindyGridworld1, n.episodes = 10000L,
#   preprocessState, predict, train,
#   double.qlearning = FALSE, experience.replay = TRUE, replay.memory = replay.memory2,
#   replay.memory.size = 20000L, batch.size = 32L, update.target.after = 100L,
#   epsilon = 0.1, epsilon.decay = 0.5, epsilon.decay.after = 100L,
#   discount.factor = 1, seed = NULL)
# 
# # double qlearning
# res = qlearning2(WindyGridworld1, n.episodes = 100L,
#   preprocessState, predict, train,
#   double.qlearning = TRUE, experience.replay = FALSE, replay.memory = NULL,
#   replay.memory.size = 10000L, batch.size = 32L, update.target.after = 100L,
#   epsilon = 0.1, epsilon.decay = 0.5, epsilon.decay.after = 100L,
#   discount.factor = 1, seed = NULL)
