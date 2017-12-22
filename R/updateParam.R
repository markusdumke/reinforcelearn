#' updateParam = function(agent, subclass, ...) {
#'  # fixme
#' }
#'
#' # updateParam(env, discount = 0.8)
#' updateParam("agent", "policy", epsilon = 0.4)
#'
#' #' Set policy
#' #'
#' #' Can be used to change the agent's policy.
#' #'
#' #' @inheritParams getValueFunction
#' #' @param new.policy \[Policy] \cr A policy object created by [makePolicy].
#' #'
#' #' @md
#' #'
#' #' @return
#' #' @export
#' setPolicy = function(agent, new.policy) {
#'   checkmate::assertClass(agent, "Agent")
#'   checkmate::assertClass(new.policy, "Policy") #fixme: add character option
#'   agent$policy = agent$initializePolicy(new.policy, agent$val.fun)
#' }
#' #'
#' #'
#' #' # fixme:
#' #' # setValueFunction
#' #' # setReplayMemory
#' #' # setAlgorithm
