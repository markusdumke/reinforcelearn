#' Create Agent.
#'
#' @param policy \[`character(1)` | Policy] \cr A policy.
#'   If you pass a string the policy will be created via [makePolicy].
#' @param val.fun \[`character(1)` | ValueFunction] \cr A value function representation.
#'   If you pass a string the value function will be created via [makeValueFunction].
#' @param algorithm \[`character(1)` | Algorithm] \cr An algorithm.
#'   If you pass a string the algorithm will be created via [makeAlgorithm].
#' @param preprocess \[`function`] \cr A function which preprocesses the state so that the agent can learn on this.
#' @param experience.replay \[`logical(1)` | ReplayMemory] \cr Replay memory for experience replay.
#' @param ... \[`any`] \cr Arguments passed on to [makePolicy], [makeAlgorithm] or [makeValueFunction].
#'
#' @md
#'
#' @export
makeAgent = function(policy, val.fun = NULL, algorithm = NULL,
  preprocess = identity, experience.replay = NULL, ...) { # better defaults?

  checkmate::assertFunction(preprocess)
  if (is.character(policy)) {
    policy = makePolicy(policy, ...)
  } else {
    checkmate::assertClass(policy, "Policy")
  }
  if (is.character(val.fun)) {
    val.fun = makeValueFunction(val.fun, ...)
  } else {
    checkmate::assertClass(val.fun, "ValueFunction", null.ok = TRUE)
  }
  if (is.character(algorithm)) {
    algorithm = makeAlgorithm(algorithm, ...)
  } else {
    checkmate::assertClass(algorithm, "Algorithm", null.ok = TRUE)
  }
  if (is.logical(experience.replay)) {
    experience.replay = makeReplayMemory(...)
  } else {
    checkmate::assertClass(experience.replay, "ReplayMemory", null.ok = TRUE)
  }

  Agent$new(policy, val.fun, algorithm, preprocess, experience.replay)
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

    learn.possible = TRUE,

    train.data = NULL,

    act2 = NULL,
    reset = NULL,
    getTarget = NULL,
    getError = NULL,

    eligibility = NULL,
    exp.replay = NULL,

    n.actions = NULL,
    n.states = NULL,

    initialized = TRUE,
    initialized.val.fun = TRUE,

    init = function(env) {
      self$n.actions = env$n.actions
      self$n.states = env$n.states
      if (self$initialized.val.fun == FALSE) {
        self$val.fun = ValueTable$new(n.states = self$n.states, n.actions = self$n.actions)
        if (!is.null(self$eligibility)) {
          self$eligibility$reset(self$val.fun$Q)
        }
        self$initialized.val.fun == TRUE
      }
    },

    # store all encountered states, actions, rewards with corresponding episode number
    # possibly write to file?
    # set maximum size for preallocation?
    history = list(),
    # logging function

    initialize = function(policy, val.fun, algorithm, preprocess,
      exp.replay) {


      if (is.null(exp.replay)) experience.replay = FALSE
      if (is.null(algorithm)) eligibility.traces = FALSE

      self$preprocess = preprocess

      self$policy = switch(policy$name,
        random = RandomPolicy$new(),
        epsilon.greedy = do.call(EpsilonGreedyPolicy$new, policy$args),
        greedy = GreedyPolicy$new(),
        softmax = SoftmaxPolicy$new()
      )

      if (policy$name == "random") {
        self$initialized = FALSE
        self$act2 = function(state) {
          policy.probs = self$policy$getActionProbs(NULL, n.actions = self$n.actions) # fixme: just use sample here?
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
        if (!any(c("initial.value", "n.states") %in% names(val.fun$args))) { # "ValueTable" %in% class(val.fun) &&
          #browser()
          self$initialized = FALSE
          self$initialized.val.fun = FALSE

        } else {
          self$val.fun = switch(val.fun$name,
            table = do.call(ValueTable$new, val.fun$args)
          )
        }
      }

      if (!is.null(algorithm)) {
        self$algorithm = switch(algorithm$name,
          qlearning = QLearning$new(),
          sarsa = Sarsa$new()#,
          # actor.critic = ActorCritic$new()
        )
      }

      if (missing(val.fun) || missing(algorithm)) {
        self$learn.possible = FALSE
      }

      if (!is.null(algorithm)) {
        if ("lambda" %in% names(algorithm$args)) {
          eligibility.traces = TRUE
          self$eligibility = Eligibility$new(algorithm$args$lambda, algorithm$args$traces)
          if (self$initialized.val.fun) self$eligibility$reset(self$val.fun$Q)
          self$reset = function() {
            self$eligibility$reset(self$val.fun$Q)
          }
        } else {
          eligibility.traces = FALSE

        }
        if (algorithm$name == "qlearning") {
          self$train.data = vector("list", 1)
          self$observe = function(state, action, reward, next.state, env) {
            state = self$preprocess(state)
            next.state = self$preprocess(next.state)
            self$train.data = list(state = state, action = action, reward = reward, next.state = next.state)
          }
          self$learn = function(env, learn) {
            res = self$getTarget(env)
            target = res[["target"]]
            target = fillTarget(res[["q.old"]], self$train.data$state, self$train.data$action, target)
            self$val.fun$train(self$train.data$state, target)
          }
          self$getTarget = function(env) {
            q.old = self$val.fun$predictQ(self$train.data$state)
            q.new = self$val.fun$predictQ(self$train.data$next.state)
            target = self$algorithm$getTarget(self$train.data$reward, q.new,
              discount = env$discount)
            list(q.old = q.old, target = target)
          }
          if (eligibility.traces) {
            self$observe = function(state, action, reward, next.state, env) {
              state = self$preprocess(state)
              next.state = self$preprocess(next.state)
              self$train.data = list(state = state, action = action, reward = reward, next.state = next.state)
              self$eligibility$increase(state, action)
            }
            self$getError = function(target, q.old, action) {
              target - q.old[action + 1L] # fixme: allow mse and other errors here
            }
            self$learn = function(env, learn) {
              res = self$getTarget(env)
              error = self$getError(target = res$target, q.old = res$q.old, action = self$train.data$action)
              self$val.fun$trainWithError(self$eligibility$E, error)
              self$eligibility$decrease(env$discount)
            }
          }
          if (experience.replay) {
            self$observe = function(state, action, reward, next.state, env) {
              state = self$action.value$preprocess(state)
              next.state = self$action.value$preprocess(next.state)
              self$exp.replay$observe(state, action, reward, next.state)
            }
          }


        }
        if (algorithm$name == "sarsa") {
          self$train.data = vector("list", 2)
          self$observe = function(state, action, reward, next.state, env) {
            self$train.data[[1]] = self$train.data[[2]]
            state = self$preprocess(state)
            next.state = self$preprocess(next.state)
            self$train.data[[2]] = list(state = state, action = action, reward = reward, next.state = next.state)
          }
          self$learn = function(env, learn) {
            if (is.null(self$train.data[[1]])) {return()}
            q.old = self$val.fun$predictQ(self$train.data[[1]]$state)
            q.new = self$val.fun$predictQ(self$train.data[[1]]$next.state)
            target = self$algorithm$getTarget(self$train.data[[1]]$reward, q.new,
              discount = env$discount, next.action = self$train.data[[2]]$action)
            target = fillTarget(q.old, self$train.data[[1]]$state, self$train.data[[1]]$action, target)
            self$val.fun$train(self$train.data[[1]]$state, target)
          }

        }

      } else {
        self$observe = function(state, action, reward, next.state, env) {} # agent observe n.actions from environment here?
        self$learn = function(env, learn) {}
      }
      if (eligibility.traces == FALSE) {
        self$reset = function() {}
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
