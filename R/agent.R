#' @export
Agent = R6::R6Class("Agent",
  public = list(
    policy = NULL,
    action.value = NULL,
    algorithm = NULL,
    replay.memory = NULL,

    learn.logical = TRUE,

    initialize = function(algorithm, action.value, policy, replay.memory) {
      self$policy = policy
      self$algorithm = algorithm
      self$action.value = action.value
      self$replay.memory = replay.memory
      # self$act = algorithm$act # fixme: bug
    },

    setLearning = NULL, # function to decide when to learn

    # fixme: this should be overwriteable ...
    act = function(state) {
      action.vals = self$action.value$predictQ(state)
      policy.probs = self$policy$getActionProbs(action.vals, length(action.vals))
      action = self$policy$sampleAction(policy.probs)
      action
    },

    observe = NULL, # exp. replay observe

    reset = function() {
    }, # e.g. reset eligibility traces to 0 after episode

    # this needs to be more flexible
    learn = function(state, action, new.state, reward, discount = 1) {
      action.vals = self$action.value$predictQ(new.state)
      target = self$algorithm$getTarget(reward, discount, action.vals)
      old.action.vals = self$action.value$predictQ(state) # this is unnecessary computation, because already computed in act!!
      target = fillTarget(old.action.vals, target, action)
      self$action.value$train(state, target) # we need the old action vals here!
    }
  )
)

fillTarget = function(old.action.vals, target, action) {
  old.action.vals[action + 1] = target
  old.action.vals
}

ValueAgent = R6::R6Class("ValueAgent",
  # inherit = Agent,
  public = list(
    act = function(state) {
      action.vals = self$action.value$predictQ(state)
      policy.probs = self$policy$getActionProbs(action.vals)
      action = self$policy$sampleAction(policy.probs)
      action
    }
  )
)

#' @export
QLearning = R6::R6Class("QLearning",
  # inherit = ValueAgent,
  public = list(
    getTarget = function(reward, discount, action.values) {
      reward + discount * max(action.values)
    },

    initialize = function() {

    }
  )
)

PolicyGradientAgent = R6::R6Class("ValueAgent",
  #inherit = Agent,
  public = list(
    act = function(state) {

    }
  )
)
