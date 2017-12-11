Agent = R6::R6Class("Agent",
  public = list(
    policy = NULL,
    action.value = NULL,
    algorithm = NULL,

    initialize = function(algorithm, action.value, policy) {
      self$policy = policy
      self$algorithm = algorithm
      self$action.value = action.value
    },

    act = function(state) {
      action.vals = self$action.value$predictQ(state)
      policy.probs = self$policy$getActionProbs(action.vals)
      action = self$policy$sampleAction(policy.probs)
      action
    },

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

# ValueAgent = R6::R6Class("ValueAgent",
#   inherit = Agent,
#   public = list(
#     value.fun = NULL
#   )
# )

QLearning = R6::R6Class("QLearning",
  # inherit = ValueAgent,
  public = list(
    getTarget = function(reward, discount, action.values) {
      reward + discount * max(action.values)
    }
  )
)

## components of agents
# state preprocessing
# explorer, e.g. epsilon.greedy
# policy
# interact with environment
# value.function (optional)
# type of fun.approx, e.g. neural network
# predict predict values given state (depends on fun.approx)
# train/fit/learn (depends on fun.approx and algorithm)
# episodic / continuous (part of env?)

## algorithm
# 1. interact
# 2. learn (optional)
# 1. and 2. include often
# - getPreprocessedState: s -> phi (preprocess function)
# - getActionValues: phi -> Q (predict)
# - getPolicy: Q -> pi (e.g. epsilon-greedy)
# - getAction pi -> action (sample from pi)
# - ... (add2Replay, sampleFromReplay ...)
# a reset when episode is done


# testAgent = agent$new()
# testAgent$getPolicy()

# target policy, behaviour policy?

# exploration: e.g. epsilon-greedy

# policyGradientAgent

# rl.exp = experiment(task, agent)

# getPolicy(value.fun) -> returns policy, e.g. softmax, epsilon-greedy, random policy
# sampleAction(policy) -> returns action sampled from policy
# interact(agent, environment) -> samples action from policy, then calls step with action
# learn function, algorithm specific, e.g. qlearning, sarsa: updates weights of value fun or policy

# keras neural network
# eligibility traces, exp. replay ...

## keras mountain car example
