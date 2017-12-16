#' Create Agent.
#'
#' @param policy \[`Policy`] \cr A policy created by [makePolicy].
#' @param val.fun \[`ActionValueTable`] \cr Value function representation.
#' @param algorithm \[`Algorithm`] \cr An algorithm.
#'
#' @md
#'
#' @export
makeAgent = function(policy, val.fun = NULL, algorithm = NULL,
  preprocess = identity, experience.replay = NULL, eligibility.traces = NULL) { # better defaults?
  checkmate::assertClass(policy, "Policy")
  checkmate::assertClass(val.fun, "ValueFunction", null.ok = TRUE)
  checkmate::assertClass(algorithm, "Algorithm", null.ok = TRUE)
  Agent$new(policy, val.fun, algorithm, preprocess, experience.replay, eligibility.traces)
}

Agent = R6::R6Class("Agent",
  public = list(
    action = NULL,
    previous.action = NULL,

    policy = NULL,
    val.fun = NULL,
    algorithm = NULL,

    preprocess = NULL,
    observe = NULL,
    learn = NULL,

    learn.logical = TRUE,

    train.data = NULL,

    act2 = NULL,

    # store all encountered states, actions, rewards with corresponding episode number
    # possibly write to file?
    # set maximum size for preallocation?
    history = list(),
    # logging function

    initialize = function(policy, val.fun, algorithm, preprocess,
      exp.replay, eligibility) {

      self$preprocess = preprocess

      self$policy = switch(policy$name,
        random = RandomPolicy$new(),
        epsilon.greedy = do.call(EpsilonGreedyPolicy$new, policy$args),
        greedy = GreedyPolicy$new(),
        softmax = SoftmaxPolicy$new()
      )

      if (policy$name == "random") {
        self$act2 = function(state) {
          policy.probs = self$policy$getActionProbs(NULL, n.actions = 4L) # fixme: dont hardcode n.actions
          action = self$policy$sampleAction(policy.probs)
        }
      } else {
        self$act2 = function(state) {
          action.vals = self$val.fun$predictQ(state)
          policy.probs = self$policy$getActionProbs(action.vals, n.actions = length(action.vals))
          action = self$policy$sampleAction(policy.probs)
        }
      }

      if (!is.null(val.fun)) { # name this model?
        self$val.fun = switch(val.fun$name,
          table = do.call(ActionValueTable$new, val.fun$args)
        )
      }

      if (!is.null(algorithm)) {
        self$algorithm = switch(algorithm$name,
          qlearning = QLearning$new(),
          sarsa = Sarsa$new(),
          actor.critic = ActorCritic$new()
        )
      }

      if (missing(val.fun) || missing(algorithm)) {
        self$learn.logical = FALSE
      }

      if (!is.null(algorithm)) {
        if (algorithm$name == "qlearning") {
          self$train.data = vector("list", 1)
          self$observe = function(state, action, reward, next.state) {
            state = self$preprocess(state)
            next.state = self$preprocess(next.state)
            self$train.data = list(state = state, action = action, reward = reward, next.state = next.state)
          }
          self$learn = function(env) {
            #browser()
            q.old = self$val.fun$predictQ(self$train.data$state)
            q.new = self$val.fun$predictQ(self$train.data$next.state)
            target = self$algorithm$getTarget(self$train.data$reward, q.new,
              discount = env$discount)
            target = fillTarget(q.old, self$train.data$state, self$train.data$action, target)
            self$val.fun$train(self$train.data$state, target)
          }
        }
        if (algorithm$name == "sarsa") {
          self$train.data = vector("list", 2)
          self$observe = function(state, action, reward, next.state) {
            self$train.data[[1]] = self$train.data[[2]]
            state = self$preprocess(state)
            next.state = self$preprocess(next.state)
            self$train.data[[2]] = list(state = state, action = action, reward = reward, next.state = next.state)
          }
          self$learn = function(env) {
            if (is.null(self$train.data[[1]])) {return()}
            q.old = self$val.fun$predictQ(self$train.data[[1]]$state)
            q.new = self$val.fun$predictQ(self$train.data[[1]]$next.state)
            target = self$algorithm$getTarget(self$train.data[[1]]$reward, q.new,
              discount = env$discount, next.action = self$train.data[[2]]$action)
            target = fillTarget(q.old, self$train.data[[1]]$state, self$train.data[[1]]$action, target)
            self$val.fun$train(self$train.data[[1]]$state, target)
          }

        }

        if (algorithm$name == "actor.critic") {
          self$observe = function(state, action, reward, next.state) {
            state = self$preprocess(state)
            next.state = self$preprocess(next.state)
            self$train.data = list(state = state, action = action, reward = reward, next.state = next.state)
          }
        }

      } else {
        self$observe = function(state, action, reward, next.state) {} # agent observe n.actions from environment here?
        self$learn = function(env) {}
      }
    },

    act = function(state) {
      state = self$preprocess(state)
      action = self$act2(state)
      self$previous.action = action
      self$action = action
      action
    }#,

 #    learn = function(env, learn) {
 # # checkLearning()
 #    }
  )
)

fillTarget = function(old.action.vals, state, action, target) {
  old.action.vals[matrix(c(seq_along(action), action + 1L), ncol = 2)] = target
  old.action.vals
}
