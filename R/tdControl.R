#' @export
#' @inheritParams qSigma
#' @rdname qSigma
qlearning = function(envir, value.function = "table", n.episodes = 100, lambda = 0, 
  learning.rate = 0.1, epsilon = 0.1, discount = 1, 
  double.learning = FALSE, replay.memory = NULL, replay.memory.size = 1, 
  batch.size = 1, alpha = 0, theta = 0.01, eligibility = "accumulate", 
  update.target.after = 1, preprocessState = NULL, model = NULL, 
  updateEpsilon = NULL, updateSigma = NULL, updateLambda = NULL, updateAlpha = NULL, 
  updateLearningRate = NULL, updateTheta = NULL, initial.value = 0) {
  
  qSigma(envir, value.function = "table", n.episodes, sigma = 0, lambda, 
    learning.rate, epsilon, discount, on.policy = FALSE,
    double.learning, replay.memory, replay.memory.size, 
    batch.size, alpha, theta, eligibility, 
    update.target.after, preprocessState, model, 
    updateEpsilon, updateSigma, updateLambda, updateAlpha, 
    updateLearningRate, updateTheta, initial.value)
}

#' @export
#' @inheritParams qSigma
#' @rdname qSigma
sarsa = function(envir, value.function = "table", n.episodes = 100, lambda = 0, 
  learning.rate = 0.1, epsilon = 0.1, discount = 1, 
  double.learning = FALSE, replay.memory = NULL, replay.memory.size = 1, 
  batch.size = 1, alpha = 0, theta = 0.01, eligibility = "accumulate", 
  update.target.after = 1, preprocessState = NULL, model = NULL, 
  updateEpsilon = NULL, updateSigma = NULL, updateLambda = NULL, updateAlpha = NULL, 
  updateLearningRate = NULL, updateTheta = NULL, initial.value = 0) {
  
  qSigma(envir, value.function = "table", n.episodes, sigma = 1, lambda, 
    learning.rate, epsilon, discount, on.policy = TRUE,
    double.learning, replay.memory, replay.memory.size, 
    batch.size, alpha, theta, eligibility, 
    update.target.after, preprocessState, model, 
    updateEpsilon, updateSigma, updateLambda, updateAlpha, 
    updateLearningRate, updateTheta, initial.value)
}
