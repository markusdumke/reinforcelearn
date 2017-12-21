#' Get weights of value function.
#'
#' Returns the weights of the value function representation of the agent.
#'
#' @param agent \[Agent] \cr An agent created by [makeAgent].
#'
#' @md
#'
#' @return For a value function table this will return a matrix, for a neural
#' network a list with the weights of the layers.
#'
#' @export
getValueFunction = function(agent) {
  checkmate::assertClass(agent, "Agent")
  if (!is.null(agent$val.fun)) {
    Q = agent$val.fun$getWeights()
  } else {
    stop("No value function weights found in the agent object.")
  }
  Q
}

#' Get replay memory.
#'
#' Returns the replay memory of the agent.
#'
#' @param agent \[Agent] \cr An agent created by [makeAgent].
#'
#' @md
#'
#' @return A list containing the experienced observations, actions and rewards.
#'
#' @export
getReplayMemory = function(agent) {
  checkmate::assertClass(agent, "Agent")
  if (!is.null(agent$exp.replay)) {
    mem = agent$exp.replay$memory
  } else {
    stop("No replay memory found in the agent object.")
  }
  mem
}

#' Get eligibility traces
#'
#' Returns the eligibility traces of the agent.
#'
#' @param agent \[Agent] \cr An agent created by [makeAgent].
#'
#' @md
#'
#' @return A matrix with the eligibility traces.
#'
#' @export
getEligibilityTraces = function(agent) {
  checkmate::assertClass(agent, "Agent")
  if (!is.null(agent$eligibility)) {
    e = agent$eligibility$E
  } else {
    stop("No eligibility traces found in the agent object.")
  }
  e
}


#' Get state values.
#'
#' Get state value function from  action value function.
#'
#' @param action.vals \[`matrix`] \cr Action value matrix.
#'
#' @md
#'
#' @export
getStateValues = function(action.vals) {
  checkmate::assertMatrix(action.vals)
  apply(action.vals, 1L, max)
}
