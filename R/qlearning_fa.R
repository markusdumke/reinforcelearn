#' Q-Learning with Function Approximation
#' 
#' To represent the Q values of how good an action is in a given state, 
#' a function approximator is used, which the user passes on to this 
#' function.
#' 
#' Three functions need to be passed on to the qlearnin algorithm:
#' * `makeFeatureVector(state_, ...)` takes the state observation from the 
#' environment and does some preprocessing (e.g. for an image 
#' greyscaling) or discretizes a continuous state space (e.g. grid 
#' tilings) and returns a vector.
#' * `predict(inputs_, ...)` returns a vector of Q values for a given 
#' preprocessed state
#' * `train(inputs_, outputs_, predictions_, ...)` updates the weights 
#' based on some learning method, e.g. gradient descent 
#' (minimizing the loss between predicted values and some true ouputs)
#'
#' @inheritParams qlearning
#' @param makeFeatureVector function: takes a state observation 
#' as input and returns a preprocessed state, e.g. a one-hot vector
#' @param predict function: predict returns vector of q values for a 
#' given preprocessed state observation
#' @param train function: train the model, update the weights
#' @param ... arguments passed on to makeFeatureVector, predict or train
#'
#' @return list with entries weights and the number of steps.per.episode
#' @export
#' @seealso [qlearning]
#'
#' @examples
#' # Define the environment
#' grid = WindyGridworld$new()
#' WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array,
#'   reward.matrix = grid$reward.matrix,
#'   terminal.states = grid$terminal.states,
#'   initial.state = 30)
#'   
#' # Define a tensorflow graph for a neural network.
#' library(tensorflow)
#' tf$reset_default_graph()
#' inputs = tf$placeholder(tf$float32, shape(1L, WindyGridworld1$n.states))
#' weights = tf$Variable(tf$random_uniform(shape(WindyGridworld1$n.states, 
#'   WindyGridworld1$n.actions), 0, 0.01))
#' Q = tf$matmul(inputs, weights)
#' 
#' nextQ = tf$placeholder(tf$float32, shape(1L, WindyGridworld1$n.actions))
#' loss = tf$reduce_sum(tf$square(nextQ - Q))
#' optimizer = tf$train$GradientDescentOptimizer(learning_rate = 0.1)
#' trainModel = optimizer$minimize(loss)
#' 
#' # initialize the session and the weights
#' sess = tf$Session()
#' sess$run(tf$global_variables_initializer())
#' 
#' # takes the state and returns a one-hot vector
#' makeFeatureVector = function(state_) {
#'   one_hot = matrix(rep(0L, WindyGridworld1$n.states), nrow = 1L)
#'   one_hot[1L, state_ + 1L] = 1L
#'   one_hot
#' }
#' 
#' # predict returns vector of q values for a given state
#' predict = function(inputs_) {
#'   sess$run(Q, feed_dict = dict(inputs = inputs_))
#' }
#' 
#' # train model, update weights, e.g. gradient descent: this is supervised learning
#' train = function(inputs_, outputs_, predictions_ = NULL) {
#'   sess$run(tuple(trainModel, weights),
#'     feed_dict = dict(inputs = inputs_, nextQ = outputs_))
#' }
#' 
#' res = qlearning_fa(WindyGridworld1, makeFeatureVector, predict, train, 
#'   n.episodes = 100, seed = 123)
#' 
qlearning_fa <- function(envir, makeFeatureVector, predict, train,
  n.episodes = 10, epsilon = 0.1, epsilon.decay = 0.5,
  discount.factor = 1, seed = NULL, ...) {
  
  if (!is.null(seed)) { set.seed(seed) }
  
  # statistics about learning behaviour: steps per episode
  steps.per.episode = rep(0L, n.episodes)
  
  for (i in seq_len(n.episodes)) {
    # print(paste("Episode:", i))
    envir$reset()
    state = envir$state
    j = 0
    
    while (envir$episode.over == FALSE) {
      j = j + 1
      features.state = makeFeatureVector(state, ...)
      Q.state = predict(features.state, ...)
      action = sample_epsilon_greedy_action(Q.state, epsilon)
      envir$step(action)
      
      next.state = envir$state
      reward = envir$reward
      
      features.next.state = makeFeatureVector(next.state, ...)
      Q.next.state = predict(features.next.state, ...)
      
      td.target =  reward + discount.factor * max(Q.next.state)
      target.Q = Q.state
      target.Q[action + 1] = td.target
      
      # train network, update weights
      weights = train(features.state, target.Q, Q.state, ...)
      
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
  list(weights = weights, steps.per.episode = steps.per.episode)
}
