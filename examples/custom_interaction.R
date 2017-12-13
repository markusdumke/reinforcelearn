env = WindyGridworld$new()
policy = RandomPolicy$new()

agent = Agent$new()

learnAfterEpisode = function(env, agent) {
  interact(env, agent, learn = FALSE, n.episodes = 1)
  agent$learn()
}
