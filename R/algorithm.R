#' @export
makeAlgorithm = function(class, ...) {
  checkmate::assertChoice(class,
    c("qlearning", "sarsa"))
  # fixme: check arguments here
  x = list(name = class, args = list(...))
  class(x) = "Algorithm"
  x
}

QLearning = R6::R6Class("QLearning",
  public = list(
    getTarget = function(reward, action.values, discount) {
      reward + discount * apply(action.values, 1L, max)
    }
  )
)

Sarsa = R6::R6Class("Sarsa",
  public = list(
    getTarget = function(reward, action.values, discount, next.action) {
      reward + discount * action.values[, next.action + 1L]
    }
  )
)

ActorCritic = R6::R6Class("ActorCritic",
  public = list(

  )
)
