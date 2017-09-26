checkArgs = function(envir, value.function, preprocessState, 
  model, initial.value, n.episodes, sigma, 
  target.policy, lambda, beta, learning.rate, 
  epsilon, discount, double.learning, update.target.after, 
  replay.memory, replay.memory.size, batch.size, alpha, theta, 
  updateEpsilon, updateSigma, updateLambda, updateAlpha, 
  updateLearningRate, updateTheta, printing) {
  
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$action.space == "Discrete")
  checkmate::assertChoice(value.function, c("table", "neural.network", "linear"))
  checkmate::assertNumber(discount, lower = 0, upper = 1)
  checkmate::assertNumber(learning.rate, lower = 0, upper = 1)
  checkmate::assertNumber(epsilon, lower = 0, upper = 1)
  checkmate::assertNumber(alpha, lower = 0, upper = 1)
  checkmate::assertNumber(theta, lower = 0, upper = 1)
  if (value.function == "table") {
    checkmate::assertMatrix(initial.value, null.ok = TRUE, any.missing = FALSE, 
      nrows = envir$n.states, ncols = envir$n.actions)
  }
  # if (value.function == "linear") {
  #   # checkmate::assertVector(initial.value, null.ok = TRUE, any.missing = FALSE)
  # }
  checkmate::assertNumber(lambda, lower = 0, upper = 1)
  checkmate::assertNumber(sigma, lower = 0, upper = 1)
  checkmate::assertNumber(beta, lower = 0, upper = 1)
  checkmate::assertInt(n.episodes, lower = 1)
  checkmate::assertInt(replay.memory.size, lower = 1)
  checkmate::assertInt(batch.size, lower = 1)
  checkmate::assertInt(update.target.after, lower = 1)
  checkmate::assertList(replay.memory, types = "list", null.ok = TRUE, any.missing = FALSE)
  if (!is.null(replay.memory)) {
    for (i in seq_along(replay.memory)) {
      checkmate::assertList(replay.memory[[i]], len = 4) 
       # names = c("state", "action", "reward", "next.state"))
      checkmate::assertInt(replay.memory[[i]][["action"]])
      checkmate::assertNumber(replay.memory[[i]][["reward"]])
    }
    
    replay.memory.size = length(replay.memory)
  }
  if (replay.memory.size < batch.size) {
    stop("Batch size must be smaller than replay memory size!")
  }
  checkmate::assertChoice(target.policy, c("greedy", "e-greedy"))
  checkmate::assertFlag(double.learning)
  checkmate::assertFunction(preprocessState, args = "state", ordered = TRUE, null.ok = TRUE)
  checkmate::assertFunction(updateEpsilon,  nargs = 2, null.ok = TRUE)
  checkmate::assertFunction(updateSigma,  nargs = 2, null.ok = TRUE)
  checkmate::assertFunction(updateLambda,  nargs = 2, null.ok = TRUE)
  checkmate::assertFunction(updateAlpha,  nargs = 2, null.ok = TRUE)
  checkmate::assertFunction(updateTheta,  nargs = 2, null.ok = TRUE)
  checkmate::assertFunction(updateLearningRate,  nargs = 2, null.ok = TRUE)
}
