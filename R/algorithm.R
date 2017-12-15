#' @export
makeAlgorithm = function(class, ...) {
  checkmate::assertChoice(class,
    c("qlearning"))
  # fixme: check arguments here
  x = list(name = class, args = list(...))
  class(x) = "Algorithm"
  x
}

QLearning = R6::R6Class("QLearning",
  # inherit = ValueAgent,
  public = list(
    getTarget = function(reward, action.values, discount) {
      reward + discount * apply(action.values, 1L, max)
    }
    # initialize = function() {
    #
    # }
  )
)
