#' # updateParam = function(class, subclass, ...) {
#' #   call = match.call()
#' #   class$discount = 0.8 # fixme
#' # }
#' #
#' # # updateParam(env, discount = 0.8)
#' # # updateParam("agent", "policy", epsilon = 0.4)
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
#'   agent$policy  = switch(new.policy$name,
#'     random = RandomPolicy$new(),
#'     epsilon.greedy = do.call(EpsilonGreedyPolicy$new, new.policy$args),
#'     greedy = GreedyPolicy$new(),
#'     softmax = SoftmaxPolicy$new()
#'   )
#' }
#'
#'
#' # fixme:
#' # setValueFunction
#' # setReplayMemory
#' # setAlgorithm
