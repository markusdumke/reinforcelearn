# fixme: logging
# fixme: control when to learn
# fixme: save all states, actions, rewards ...
# fixme: return number of steps, returns etc
# fixme: print out average return of last n episodes ...
interact = function(environment, agent, n.steps = 1L, n.episodes = Inf,
  max.steps.per.episode = 1000L, learn = TRUE, visualize = FALSE) {

  episode.return = 0
  episode = 0L
  step = 0L
  step.in.episode = 0L

  if (is.null(environment$state)) {
    state = environment$reset()
    environment$visualize()
  } else {
    state = environment$state
  }

  while (TRUE) {
    step = step + 1L
    step.in.episode = step.in.episode + 1L

    action = agent$act(state) # fixme: store action also in agent attribute
    res = environment$step(action)
    if (visualize) {
      environment$visualize()
    }
    episode.return = episode.return + res$reward # no discounting here yet
    if (learn) {
      agent$learn(state, action, res$state, res$reward)
    }
    state = res$state # set state to next state for new iteration

    if (res$done || step.in.episode  == max.steps.per.episode) {
      step.in.episode = 0L
      episode = episode + 1L
      environment$n.episodes = environment$n.episodes + 1L
      message(paste("Episode", environment$n.episodes, "finished after",
        environment$n.steps, "steps with a return of", episode.return))
      episode.return = 0
      #if (learn) {
      state = environment$reset()
      #}
    }

    # stop criteria
    if (step == n.steps || episode == n.episodes) {
      break
    }
  }
  # list(returns = episode.returns)
}
