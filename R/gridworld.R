#' #' Gridworld
#' #'
#' #' Simple gridworld environment for reinforcement learning.
#' #' Grid of shape 4x4 with two terminal states in the upper left and
#' #' bottom right of the grid. Episodes start in any of the nonterminal
#' #' states. The possible actions include going left, right, up and down.
#' #'
#' #' The gridworld was created with
#' #' # gridworld = makeGridworld()
#' #' # gridworld = makeEnvironment(transition.array = gridworld$transition.array,
#' #' # reward.matrix = gridworld$reward.matrix)
#' #' @format R6 class
#' #' @name
#' "gridworld"
