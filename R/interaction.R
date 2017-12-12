#' @export
interact = function(env, agent, n.steps = Inf, n.episodes = Inf,
  max.steps.per.episode = Inf, visualize = FALSE) {

  # one of steps, episodes must be finite!
  if (is.infinite(n.steps) && is.infinite(n.episodes)) {
    stop("Specify finite number of steps or number of episodes!")
  }

  # preallocation if number of episodes is known in advance
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

  episode = 0L

  # stop if specified episode or step is reached
  stop.step = env$n.step + n.steps
  stop.episode = env$episode + n.episodes

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
    # agent$observe() # observe before act
    action = agent$act(state) # fixme: store action also in agent attribute
    res = env$step(action)
    if (visualize) {
      env$visualize()
    }

    if (agent$learn.logical) {
      agent$learn(state, action, res$state, res$reward)
    }
    state = res$state # set state to next state for new iteration

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
      # agent$reset()
    }

    # stop criteria
    if (env$n.step == stop.step || env$episode == stop.episode) {
      break
    }
  }
  list(returns = episode.returns, steps = episode.steps)
}
# fixme: logging
# fixme: control when to learn
# fixme: save history of all states, actions, rewards ...
# fixme: return number of steps, returns etc
# fixme: print out average return of last n episodes ...
