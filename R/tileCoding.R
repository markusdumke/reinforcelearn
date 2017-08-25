#' Tile Coding
#' 
#' Tile Coding for a two-dimensional state space with uniformly offset tilings.
#' 
#' @param x [\code{numeric(1)}] \cr x: first dimension of the state.
#' @param y [\code{numeric(1)}] \cr y: second dimension of the state.
#' @param n.tilings [\code{integer(1)}] \cr Number of tilings.
#' @param n.tiles [\code{integer(1)}] \cr Number of tiles per tiling.
#' @param tilemove.x [\code{numeric(1)}] \cr Tile offset in x direction.
#' @param tilemove.y [\code{numeric(1)}] \cr Tile offset in y direction.
#' @param lower.bound.x [\code{numeric(1)}] \cr Lower bound of x.
#' @param lower.bound.y [\code{numeric(1)}] \cr Lower bound of y.
#' @param upper.bound.x [\code{numeric(1)}] \cr Upper bound of x.
#' @param upper.bound.y [\code{numeric(1)}] \cr Upper bound of y.
#'
#' @return [\code{vector(n.tilings)}]: the tile indices which are active.
#' @export
#' @references Sutton & Barto (2017): Reinforcement Learning: An Introduction
#'
#' @examples
#' tilecode(x = 0.3, y = 0.1, n.tilings = 2, n.tiles = 4, tilemove.x =  - 0.4 / 2, 
#'   tilemove.y = - 0.4 / 2, lower.bound.x = 0, lower.bound.y = 0, 
#'   upper.bound.x = 1, upper.bound.y = 1)
#' 
#' \dontrun{
#' MountainCar = makeEnvironment("MountainCar-v0")
#' MountainCar$reset()
#' bounds = MountainCar$state.space.bounds
#' tilecode(MountainCar$state[[1]], MountainCar$state[[2]], n.tilings = 8, n.tiles = 16, 
#'   tilemove.x = - 0.2125 / n.tilings, tilemove.y = - 0.0175 / n.tilings, 
#'   lower.bound.x = bounds[[1]][1], lower.bound.y = bounds[[2]][1], 
#'   upper.bound.x = bounds[[1]][2], upper.bound.y = bounds[[2]][2])
#' }
tilecode = function(x, y, n.tilings, n.tiles, tilemove.x, tilemove.y, 
  lower.bound.x, lower.bound.y, upper.bound.x, upper.bound.y) {
  
  x = x - lower.bound.x
  y = y - lower.bound.y
  tileIndices = rep(0, n.tilings)
  
  for (i in seq_len(n.tilings)) {
    x.move = (i - 1) * tilemove.x
    y.move = (i - 1) * tilemove.y
    x.scale = (x - x.move) / (abs(lower.bound.x) + abs(upper.bound.x) + abs(tilemove.x) * n.tilings)
    y.scale = (y - y.move) / (abs(lower.bound.x) + abs(upper.bound.x) + abs(tilemove.y) * n.tilings)
    xcoord = ceiling(x.scale * (n.tiles / 2))
    ycoord = ceiling(y.scale * (n.tiles / 2))
    tileIndices[i] = (i - 1) * n.tiles + ((ycoord - 1) * (n.tiles / 2) + xcoord)
  }
  tileIndices
}

#' Make n hot vector
#' @param x [\code{integer}] \cr Which features are active?
#' @param len [\code{integer(1)}] \cr Length of the feature vector.
#' @return [\code{matrix(1, len)}] A one-row matrix with \code{len} columns with every entry 0 
#'   except the columns specified by \code{x}.
#' @export
#' @examples 
#' makeNHot(c(1, 3), 5)
makeNHot = function(x, len) {
  m = matrix(rep(0, len), nrow = 1)
  m[1, x] = 1
  m
}
