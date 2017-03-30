#' Make one-hot vector
#'
#' @param hot scalar integer: which entry is 1 (numeration starting from 0)
#' @param len scalar integer: the length of the one-hot vector
#' @param matrix logical: should the output be a matrix with one row 
#'
#' @return one_hot vector or matrix
#' @export
#' @examples 
#' make_one_hot_vector(0, 10)
make_one_hot_vector <- function(hot, len, matrix = TRUE) {
  one_hot = matrix(rep(0, len), nrow = 1)
  one_hot[1, hot + 1] = 1
  one_hot
}

#' Q-Network Tensorflow
#' 
#' Simple neural network tensorflow implementation
#'
#' @inheritParams predictMC
#' @inheritParams sarsa
#' @param make_feature_vector function which returns a 
#' feature vector for a given state observation.
#' @param ... arguments passed to make_feature_vector
#' @param loss.function scalar character: a valid name of a loss function from 
#' tensorflow, e.g. "mean_squared_error" or "softmax_cross_entropy"
#'
#' @return list with entries weights and episode.finished.after the 
#' number of time steps each episode needed
#' @export
#' @import tensorflow
#'
#' @examples
#' grid = WindyGridworld$new()
#' WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array,
#'   reward.matrix = grid$reward.matrix,
#'   terminal.states = grid$terminal.states,
#'   initial.state = grid$initial.state)
#'   
#' res = qnetwork(WindyGridworld1, make_one_hot_vector, 
#'   n.episodes = 300, len = 70)
#' plot(1:500, res$episode.finished.after[1:500], ylim = c(0, 200), 
#'   type = "l", xlab = "Episode", ylab = "Steps per Episode")
#' abline(h = 15, col = "red") # optimal solution
#' 
qnetwork <- function(envir, make_feature_vector, n.episodes = 10,
  epsilon = 0.1, epsilon.decay = 0.5, learning.rate = 0.1, 
  discount.factor = 1, loss.function = "mean_squared_error", ...) {
  
  tf$reset_default_graph()
  
  # initialize variables
  # These lines establish the feed-forward part of the network used to choose actions
  inputs1 = tf$placeholder(tf$float32, shape(1L, envir$n.states)) # input is a one-hot vector
  W = tf$Variable(tf$random_uniform(shape(envir$n.states, envir$n.actions), 0, 0.01)) # weights matrix
  Q = tf$matmul(inputs1, W) # matrix multiplication to estimate Q values for each action
  
  # Below we obtain the loss by taking the sum of squares difference
  #   between the target and prediction Q values.
  nextQ = tf$placeholder(tf$float32, shape(1L, envir$n.actions)) # next Q value (target)
  loss_char = paste0("tf$contrib$losses$", loss.function, "(Q, nextQ)")
  loss = eval(parse(text = loss_char))
  # loss = tf$reduce_sum(tf$square(nextQ - Q)) # MSE between old (predictions) and new Q values (targets)
  trainer = tf$train$GradientDescentOptimizer(learning_rate = learning.rate) # Gradient Descent Optimization
  updateModel = trainer$minimize(loss) # minimize loss
  
  sess = tf$Session() # start session
  sess$run(tf$global_variables_initializer()) # initialize variables
  
  # statistics about learning behaviour: steps per episode
  steps.per.episode = rep(0L, n.episodes)
  
  for (i in seq_len(n.episodes)) {
    print(paste("Episode:", i))
    envir$reset()
    state = envir$state
    j = 0
    
    while (envir$episode.over == FALSE) {
      j = j + 1
      features.state = make_feature_vector(state, ...)
      Q.state = sess$run(Q, feed_dict = dict(inputs1 = features.state)) # qnetwork
      action = sample_epsilon_greedy_action(Q.state, epsilon)
      envir$step(action)
      
      next.state = envir$state
      reward = envir$reward
      
      features.next.state = make_feature_vector(next.state, ...)
      Q.next.state = sess$run(Q, feed_dict = dict(inputs1 = features.next.state))
      
      td.target =  reward + discount.factor * max(Q.next.state)
      target.Q = Q.state
      target.Q[action + 1] = td.target
      
      # train network, update weights
      W1 = sess$run(tuple(updateModel, W), feed_dict = dict(inputs1 = features.state, nextQ = target.Q))
      
      state = next.state
      
      if (envir$episode.over) {
        if (i %% 100 == 0) {
          epsilon = epsilon * epsilon.decay
        }
        steps.per.episode[i] = j
        print(paste("Episode", i, "finished after", j, "time steps."))
        break
      }
    }
  }
  list(weights = W1, steps.per.episode = steps.per.episode)
}
