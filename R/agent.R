#' @export
Agent = R6::R6Class("Agent",
  public = list(
    policy = NULL,
    action.value = NULL,
    algorithm = NULL,
    exp.replay = NULL,

    observe = NULL,
    getLearnData = NULL,
    learn = NULL,

    learn.logical = TRUE,

    # store all encountered states, actions, rewards with corresponding episode number
    # possibly write to file?
    # set maximum size for preallocation?
    history = list(),
    # logging function

    initialize = function(algorithm, action.value, policy, replay.memory) {
      self$policy = policy
      self$algorithm = algorithm
      self$action.value = action.value # better: make copy of action.value?!

      if (missing(replay.memory)) {
        self$exp.replay = ExperienceReplay$new(size = 1L, batch.size = 1L)
      } else {
        self$exp.replay = replay.memory
      }

      self$observe = function(state, action, reward, next.state) {
        state = self$action.value$preprocess(state)
        next.state = self$action.value$preprocess(next.state)
        self$exp.replay$observe(state, action, reward, next.state)
      }

      self$getLearnData = function() {
        data = self$exp.replay$sampleBatch()
        data
      }

      # this needs to be more flexible
      if ("ActionValueTable" %in% class(action.value)) {
        self$learn = function(discount = 1) {
          data = self$getLearnData()
          if (!is.null(data)) {
            # browser()
            data = self$action.value$processBatch(data)
            data$target = self$getTarget(data, discount)
            # sum together updates to the same state-action pair
            data = aggregate(target ~ state + action, data = data, FUN = sum)
            val.old = self$action.value$predictQ(data$state)
            target = fillTarget(val.old, data$state, data$action, data$target)
            target = as.data.frame(target)
            target$state = data$state
            # sum together updates to same state
            target = aggregate(. ~ state, data = target, FUN = sum)

            self$action.value$train(target$state, as.matrix(target[-1]))
          }
        }
      }

      if ("ActionValueNetwork" %in% class(action.value)) {
        self$learn = function(discount = 1) {
          data = self$getLearnData()
          if (!is.null(data)) {
            #browser()
            data = self$action.value$processBatch(data) # this is copy paste
            data$target = self$getTarget(data, discount)

            val.old = self$action.value$predictQ(data$state)
            target = fillTarget(val.old, data$state, data$action, data$target)

            self$action.value$train(data$state, target)
          }
        }
      }

    },

    setLearning = NULL, # function to decide when to learn

    # fixme: this should be overwriteable ...
    act = function(state) {
      state = self$action.value$preprocess(state)
      action.vals = self$action.value$predictQ(state) # unnecessary for e.g. random policy!
      policy.probs = self$policy$getActionProbs(action.vals, length(action.vals))
      action = self$policy$sampleAction(policy.probs)
      action
    }, # if policy == RandomPolicy then act = ... else act = ...

    reset = function() {
    }, # e.g. reset eligibility traces to 0 after episode

    getTarget = function(data, discount) {
      val.new = self$action.value$predictQ(data$next.state)
      target = self$algorithm$getTarget(data$reward, val.new, discount)
      target
    },

    getWeights = function() {
      self$action.value$getWeights()
    },

    getMemory = function() {
      agent$exp.replay$memory
    }

    # setLearningRate

  )
)

fillTarget = function(old.action.vals, state, action, target) {
  old.action.vals[matrix(c(seq_along(action), action + 1L), ncol = 2)] = target
  old.action.vals
}

# ValueAgent = R6::R6Class("ValueAgent",
#   # inherit = Agent,
#   public = list(
#     act = function(state) {
#       action.vals = self$action.value$predictQ(state)
#       policy.probs = self$policy$getActionProbs(action.vals)
#       action = self$policy$sampleAction(policy.probs)
#       action
#     }
#   )
# )

#' @export
QLearning = R6::R6Class("QLearning",
  # inherit = ValueAgent,
  public = list(
    getTarget = function(reward, action.values, discount) {
      reward + discount * apply(action.values, 1L, max)
    }
    # initialize = function() {
    #
    # }
  )
)

# PolicyGradientAgent = R6::R6Class("ValueAgent",
#   #inherit = Agent,
#   public = list(
#     act = function(state) {
#
#     }
#   )
# )
