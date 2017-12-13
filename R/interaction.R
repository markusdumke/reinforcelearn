#' @export
interact = function(env, agent, n.steps = Inf, n.episodes = Inf,
  max.steps.per.episode = Inf, visualize = FALSE) {

  # one of steps / episodes must be finite!
  if (is.infinite(n.steps) && is.infinite(n.episodes)) {
    stop("Specify finite number of steps or finite number of episodes!")
  }

  # preallocation if number of episodes is known in advance else append to list
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

  # stop if specified episode or step is reached
  stop.step = env$n.step + n.steps
  stop.episode = env$episode + n.episodes

  # check if environment has been resetted, if not reset else get current state
  if (is.null(env$state)) {
    message("Reset environment.")
    state = env$reset()
    if (visualize) {
      env$visualize()
    }
  } else {
    state = env$state
  }

  while (TRUE) {
    # # for debugging
    # print(paste0("episode: ", env$episode, "; step: ", env$n.step))
    # if (env$n.step == 2) browser()
    # # agent$observeBeforeAct() # observe before act
    action = agent$act(state) # fixme: store action also in agent attribute
    res = env$step(action)

    # # keep track of visited states, actions, rewards
    # agent$history = append(agent$history, list(list(state = state, action = action,
    #   reward = res$reward, episode = env$episode + 1L)))

    # visualization, maybe wrap this into env$step?
    if (visualize) {
      env$visualize()
    }

    # observe: e.g. add observation to replay memory
    agent$observe(state, action, res$reward, res$state)

    # optional learning (check whether to learn maybe as agent method)
    if (agent$learn.logical) {
      agent$learn(discount = env$discount)
    }

    state = res$state # set state to next state for new iteration

    # when episode is finished print out information and reset environment
    if (res$done || env$episode.step == max.steps.per.episode) {
      if (!res$done) {
        env$episode = env$episode + 1L
      }
      message(paste("Episode", env$episode, "finished after",
        env$episode.step, "steps with a return of", env$episode.return))
      episode = episode + 1L
      episode.returns[episode] = env$episode.return
      episode.steps[episode] = env$episode.step
      state = env$reset()
      if (visualize) {
        env$visualize()
      }
      # agent$reset()
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
