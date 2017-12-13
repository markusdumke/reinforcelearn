algorithm = QLearning$new()

val = ActionValueTable$new(5, 5)
val$Q

memory = list(list(state = 1L, action = 1L, reward = -1, next.state = 2L),
  list(state = 2L, action = 0L, reward = 4, next.state = 2L))

batch = list(state = list(1, 1, 1), action = list(2, 2, 1),
  reward = list(-1, 4, -2), next.state = list(1, 2, 2))

processBatch = function(batch) {
  data = data.frame(state = unlist(batch[["state"]]), action = unlist(batch[["action"]]),
    reward = unlist(batch[["reward"]]), next.state = unlist(batch[["next.state"]]))
  data
}

data = processBatch(batch)

# we only need predictions for unique states and unique next.states
getTarget = function(data) {
  val.new = val$predictQ(data$next.state)
  target = algorithm$getTarget(data$reward, val.new, discount = 1)
  target
}

data$target = getTarget(data)
# sum together updates to the same state, action pair
data = aggregate(target ~ state + action, data = data, FUN = sum)

val.old = val$predictQ(data$state)

fillTarget = function(old.action.vals, state, action, target) {
  old.action.vals[matrix(c(seq_along(action), action + 1L), ncol = 2)] = target
  old.action.vals
}

target = fillTarget(val.old, data$state, data$action, data$target)
target = as.data.frame(target)
target$state = data$state
# sum together updates to same state
target = aggregate(. ~ state, data = target, FUN = sum)

val$train(target$state, as.matrix(target[-1]))
val$Q
