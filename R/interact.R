#' Interaction between agent and environment.
#'
#' Run interaction between agent and environment for specified number of steps
#' or episodes.
#'
#' @param env \[`Environment`] \cr Reinforcement learning environment created by [makeEnvironment].
#' @param agent \[`Agent`] \cr Agent created by [makeAgent].
#' @param n.steps \[`integer(1)`] \cr Number of steps to run.
#' @param n.episodes \[`integer(1)`] \cr Number of episodes to run.
#' @param max.steps.per.episode \[`integer(1)`] \cr Maximal number of steps allowed per episode.
#' @param learn \[`logical(1)`] \cr Should the agent learn?
#' @param visualize \[`logical(1)`] \cr Visualize the interaction between agent and environment?
#'
#' @return \[`list`] Return and number of steps per episode.
#'
#' @md
#'
#' @export
#' @examples
#' env = makeEnvironment("windy.gridworld")
#' agent = makeAgent("softmax", "table", "qlearning")
#' interact(env, agent, n.episodes = 10L)
interact = function(env, agent, n.steps = Inf, n.episodes = Inf,
  max.steps.per.episode = Inf, learn = TRUE, visualize = FALSE) {

  checkmate::assertClass(env, "Environment")
  checkmate::assertClass(agent, "Agent")
  if (!is.infinite(n.steps)) checkmate::assertInt(n.steps, lower = 1)
  if (!is.infinite(n.episodes)) checkmate::assertInt(n.episodes, lower = 1)
  if (!is.infinite(max.steps.per.episode)) checkmate::assertInt(max.steps.per.episode, lower = 1)
  checkmate::assertFlag(learn)
  checkmate::assertFlag(visualize)

  # one of steps / episodes must be finite!
  if (is.infinite(n.steps) && is.infinite(n.episodes)) {
    stop("Specify finite number of steps or finite number of episodes!")
  }

  # preallocation if number of episodes | steps is known in advance else append to list
  if (n.episodes < Inf) {
    episode.returns = rep(NA_real_, n.episodes)
  } else {
    episode.returns = vector(mode = "double")
  }
  if (n.episodes < Inf) {
    episode.steps = rep(NA_integer_, n.episodes)
  } else {
    episode.steps = vector(mode = "integer")
  }

  # index to fill in
  episode = 0L

  # get episode | step number of when to stop
  stop.step = env$n.step + n.steps
  stop.episode = env$episode + n.episodes

  # # check if environment has been resetted, if not reset else get current state
  # if (is.null(env$state)) {
  #   message("Reset environment.")
  #   state = env$reset()
  #   if (visualize) {
  #     env$visualize()
  #   }
  # } else {
  state = env$state
  #}

  agent$n.actions = env$n.actions

  if (agent$initialized == FALSE) {
    agent$init(env) # if e.g. value fun has not been initialized do this here
    agent$initialized = TRUE
  }

  while (TRUE) {
    # print(paste0("episode: ", env$episode, "; step: ", env$n.step))
    # # agent$observeBeforeAct() # observe before act
    action = agent$act(state) # fixme: store action also in agent attribute
    res = env$step(action)

    if (visualize) {
      env$visualize()
    }

    # # keep track of visited states, actions, rewards
    # agent$history = append(agent$history, list(list(state = state, action = action,
    #   reward = res$reward, episode = env$episode + 1L)))

    # observe: e.g. add observation to replay memory
    agent$observe(state, action, res$reward, res$state, env)

    # optional learning (check whether to learn maybe as agent method)
    if (learn) {
      #browser()
      agent$learn(env, learn)
    }

    state = res$state # set state to next state for new iteration

    # when episode is finished print out information and reset environment
    if (res$done || env$episode.step == max.steps.per.episode) {
      if (!res$done) {
        env$episode = env$episode + 1L
      }
      message(paste("Episode", env$episode, "finished after",
        env$episode.step, "steps with a return of", env$episode.return)) # let this be customizable by having his in a function argument
      episode = episode + 1L
      episode.returns[episode] = env$episode.return
      episode.steps[episode] = env$episode.step
      state = env$reset()
      # if (visualize) {
      #   env$visualize()
      # }
      agent$reset()
    }

    # stop criteria
    if (env$n.step == stop.step || env$episode == stop.episode) {
      break
    }
  }
  # return information about returns, steps
  list(returns = episode.returns, steps = episode.steps) # return history
}
# fixme: logging
# fixme: control when to learn
# fixme: print out average return of last n episodes ...
# fixme: maybe return training time, history ...
# make message after done configurable as function argument
