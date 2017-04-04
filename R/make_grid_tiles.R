#' Make feature vector using grid tiles
#' 
#' Tiles: Grid across the 2-dimensional state space: each observation
#' will be encoded as a one-hot vector.
#'
#' @param state length-two list: the state observation
#' @param state.space.bounds length-two list: the state space bounds
#' @param n.grid integer scalar: sqrt of number of grid cells
#'
#' @return one-hot feature vector
#' @export
makeGridTiling = function(state, state.space.bounds, n.grid) {
  x = state[[1]]
  y = state[[2]]

  seq_x = seq(state.space.bounds[[1]][1], state.space.bounds[[1]][2],
    length.out = n.grid + 1)
  seq_y = seq(state.space.bounds[[2]][1], state.space.bounds[[2]][2],
    length.out = n.grid + 1)
  grid = matrix(0, nrow = n.grid, ncol = n.grid)

  grid[findInterval(x, seq_x), findInterval(y, seq_y)] = 1
  grid = c(grid)
  matrix(grid, nrow = 1)
}
