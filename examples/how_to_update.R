# how to learn
batch = list(state = list(1, 1, 1), action = list(2, 2, 1))
batch

d = data.frame(state = 1, action = c(2, 2, 1), target = -1)
d
# sum together updates to the same state, action pair
d = aggregate(target ~ state + action, data = d, FUN = sum)
d

old.vals = matrix(3, ncol = 2, nrow = 2)
old.vals

fillTarget = function(old.action.vals, state, target, action) {
  old.action.vals[matrix(c(state, action), ncol = 2)] = target
  old.action.vals
}
target = fillTarget(old.vals, d$state, d$target, d$action)
target

new.vals = old.vals + 0.1 * (target - old.vals)
new.vals
