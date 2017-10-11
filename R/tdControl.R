#' @export
#' @inheritParams qSigma
#' @rdname qSigma
qlearning = function(envir, fun.approx = "table", preprocessState = NULL, 
  model = NULL, initial.value = NULL, n.states = NULL, n.episodes = 100,
  lambda = 0, beta = 0, learning.rate = 0.1, 
  epsilon = 0.1, discount = 1, double.learning = FALSE, update.target.after = 1, 
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01, 
  updateEpsilon = NULL, updateSigma = NULL, updateLambda = NULL, updateAlpha = NULL, 
  updateLearningRate = NULL) {
  
  qSigma(envir, fun.approx, preprocessState,   
    model, initial.value, n.states, n.episodes, sigma = 0, 
    target.policy = "greedy", lambda, beta, learning.rate, 
    epsilon, discount, double.learning, update.target.after, 
    replay.memory, replay.memory.size, batch.size, alpha, theta, 
    updateEpsilon, updateSigma, updateLambda, updateAlpha, 
    updateLearningRate)
}

#' @export
#' @inheritParams qSigma
#' @rdname qSigma
sarsa = function(envir, fun.approx = "table", preprocessState = NULL, 
  model = NULL, initial.value = NULL, n.states = NULL, n.episodes = 100, 
  lambda = 0, beta = 0, learning.rate = 0.1, 
  epsilon = 0.1, discount = 1, double.learning = FALSE, update.target.after = 1, 
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01, 
  updateEpsilon = NULL, updateSigma = NULL, updateLambda = NULL, updateAlpha = NULL, 
  updateLearningRate = NULL) {
  
  qSigma(envir, fun.approx, preprocessState, 
    model, initial.value, n.states, n.episodes, sigma = 1, 
    target.policy = "e-greedy", lambda, beta, learning.rate, 
    epsilon, discount, double.learning, update.target.after, 
    replay.memory, replay.memory.size, batch.size, alpha, theta, 
    updateEpsilon, updateSigma, updateLambda, updateAlpha, 
    updateLearningRate)
}

#' @export
#' @inheritParams qSigma
#' @rdname qSigma
expectedSarsa = function(envir, fun.approx = "table", preprocessState = NULL, 
  model = NULL, initial.value = NULL, n.states = NULL, n.episodes = 100, target.policy = "e-greedy",
  lambda = 0, beta = 0, learning.rate = 0.1, 
  epsilon = 0.1, discount = 1, double.learning = FALSE, update.target.after = 1, 
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01, 
  updateEpsilon = NULL, updateSigma = NULL, updateLambda = NULL, updateAlpha = NULL, 
  updateLearningRate = NULL) {
  
  qSigma(envir, fun.approx, preprocessState, 
    model, initial.value, n.states, n.episodes, sigma = 0, target.policy,
    lambda, beta, learning.rate, 
    epsilon, discount, double.learning, update.target.after, 
    replay.memory, replay.memory.size, batch.size, alpha, theta, 
    updateEpsilon, updateSigma, updateLambda, updateAlpha, 
    updateLearningRate)
}

