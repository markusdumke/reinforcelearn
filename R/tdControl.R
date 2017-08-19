#' @export
#' @inheritParams qSigma
#' @rdname qSigma
qlearning = function(envir, value.function = "table", lambda = 0, 
  n.episodes = 100, learning.rate = 0.1, epsilon = 0.1, epsilon.decay = 0.5, 
  epsilon.decay.after = 100, initial.value = 0, discount.factor = 1, 
  double.learning = FALSE, replay.memory = NULL, 
  replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01, 
  model = NULL, preprocessState = NULL, update.target.after = 1) {
  
  qSigma(envir, value.function, sigma = 0, lambda, 
    n.episodes, learning.rate, epsilon, epsilon.decay, 
    epsilon.decay.after, initial.value, discount.factor, 
    on.policy = FALSE, double.learning, replay.memory, 
    replay.memory.size, batch.size, alpha, theta, 
    model, preprocessState, update.target.after)
}

#' @export
#' @inheritParams qSigma
#' @rdname qSigma
sarsa = function(envir, value.function = "table", lambda = 0, 
  n.episodes = 100, learning.rate = 0.1, epsilon = 0.1, epsilon.decay = 0.5, 
  epsilon.decay.after = 100, initial.value = 0, discount.factor = 1, 
  double.learning = FALSE, replay.memory = NULL, 
  replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01, 
  model = NULL, preprocessState = NULL, update.target.after = 1) {
  
  qSigma(envir, value.function, sigma = 1, lambda, 
    n.episodes, learning.rate, epsilon, epsilon.decay, 
    epsilon.decay.after, initial.value, discount.factor, 
    on.policy = TRUE, double.learning, replay.memory, 
    replay.memory.size, batch.size, alpha, theta, 
    model, preprocessState, update.target.after)
}

#' @export
#' @inheritParams qSigma
#' @rdname qSigma
expectedSarsa = function(envir, value.function = "table", lambda = 0, 
  n.episodes = 100, learning.rate = 0.1, epsilon = 0.1, epsilon.decay = 0.5, 
  epsilon.decay.after = 100, initial.value = 0, discount.factor = 1, 
  double.learning = FALSE, replay.memory = NULL, 
  replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01, 
  model = NULL, preprocessState = NULL, update.target.after = 1) {
  
  qSigma(envir, value.function, sigma = 0, lambda, 
    n.episodes, learning.rate, epsilon, epsilon.decay, 
    epsilon.decay.after, initial.value, discount.factor, 
    on.policy = TRUE, double.learning, replay.memory, 
    replay.memory.size, batch.size, alpha, theta, 
    model, preprocessState, update.target.after)
}
