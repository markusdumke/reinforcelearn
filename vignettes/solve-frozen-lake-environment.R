## ---- eval = FALSE-------------------------------------------------------
#  library(reinforcelearn)
#  FrozenLake = makeEnvironment("FrozenLake-v0")

## ---- results = "hide", eval = FALSE-------------------------------------
#  n.episodes = 1000
#  res = qlearning(FrozenLake, n.episodes = n.episodes)
#  Q = res$Q

## ---- eval = FALSE-------------------------------------------------------
#  optimal.policy = max.col(Q)

## ---- eval = FALSE-------------------------------------------------------
#  plot(x = seq_len(n.episodes), y = res$rewards.per.episode / 1:n.episodes,
#    type = "l", xlim = c(0, 10000), ylab = "Average Reward", xlab = "Episode",
#    main = "Average Reward of all episodes up to now")

