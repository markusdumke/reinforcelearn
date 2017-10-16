#' Tile Coding
#' 
#' Implementation of Sutton's tile coding software version 3.
#' 
#' @param iht [\code{IHT class}] \cr
#'   A hash table created with \code{IHT}.
#' @param n.tilings [\code{integer(1)}] \cr
#'   Number of tilings.
#' @param state [\code{vector(2)}] \cr
#'   A two-dimensional state observation. 
#'   Make sure to scale the observation to unit variance before.
#' @param action [\code{integer(1)}] \cr
#'   Optional: If supplied the action space will also be tiled. 
#'   All distinct actions will result in different tiles.
#' 
#' @return  \code{IHT} creates a hash table, which can then be passed on to \code{tiles}.
#' \code{tiles} returns a vector of size \code{n.tilings} with the active tile numbers.
#' 
#' @details 
#' Tile coding is a way of representing the values of a vector of continuous variables as a large 
#' binary vector with few 1s and many 0s. The binary vector is not represented explicitly, 
#' but as a list of the components that are 1s. The main step is to partition, or tile, 
#' the continuous space multiple times and select one tile from each tiling, that corresponding 
#' the the vector's value. Each tile is converted to an element in the big binary vector, 
#' and the list of the tile (element) numbers is returned as the representation of the vector's value.
#' Tile coding is recommended as a way of applying online learning methods to domains with continuous 
#' state or action variables. [copied from manual]
#' 
#' See detailed manual at \url{http://incompleteideas.net/sutton/tiles/tiles3.html}. 
#' In comparison to the Python implementation indices start with 1 instead of 0. The hash table is 
#' implemented as an environment, which is an attribute of an R6 class.
#' 
#' Make sure that the size of the hash table is large enough, else an error will be triggered, 
#' when trying to assign a value to a full hash table.
#' 
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @rdname tilecoding
#' @export
#' @examples
#' # Create hash table
#' iht = IHT(1024)
#' 
#' # Partition state space using 8 tilings
#' tiles(iht, n.tilings = 8, state = c(3.6, 7.21))
#' tiles(iht, n.tilings = 8, state = c(3.7, 7.21))
#' tiles(iht, n.tilings = 8, state = c(4, 7))
#' tiles(iht, n.tilings = 8, state = c(- 37.2, 7))
#' 
tiles = function(iht, n.tilings, state, action = integer(0)) {
  checkmate::assertClass(iht, "IHT")
  checkmate::assertInt(n.tilings)
  checkmate::assertVector(state)
  checkmate::assertIntegerish(action, max.len = 1)
  
  qfloats = floor(state * n.tilings)
  active.tiles = rep(0, n.tilings)
  coords = rep(0, length(state) + 1)
  
  for (tiling in seq_len(n.tilings)) {
    tiling = tiling - 1
    tiling2 = tiling * 2
    coords[1] = tiling
    b = tiling
    for (q in seq_along(qfloats)) {
      coords[q + 1] = (qfloats[q] + b) %/% n.tilings
      b = b + tiling2
    }
    coords = append(coords, action)
    active.tiles[tiling + 1] = hashcoords(paste(coords, collapse = ""), iht)
  }
  
  return(active.tiles)
}

hashcoords = function(coords, iht) {
  iht$add2Env(coords)
  iht$checkFull()
  iht$getIndex(coords)
}

#' @rdname tilecoding
#' @param max.size [\code{integer(1)}] \cr 
#'   Maximal size of hash table.
#' @export
IHT = function(max.size) {
  IHTClass$new(max.size)
}

IHTClass = R6::R6Class("IHT", public = list(
  i = 0,
  max.size = NULL,
  e = NULL,
  
  initialize = function(max.size) {
    checkmate::assertInt(max.size)
    self$max.size = max.size
    self$e = new.env(size = max.size)
  },
  
  checkFull = function() {
    if (length(self$e) > self$max.size) {
      stop("Tile Coding failed because hash table IHT is full!")
    } 
  },
  
  add2Env = function(coords) {
    if (!exists(coords, envir = self$e, inherits = FALSE)) {
      self$i = self$i + 1
      self$checkFull()
      self$e[[coords]] = self$i
    }
  },
  
  getIndex = function(coords) {
    return(self$e[[coords]])
  }
)
)

#' Make n hot vector.
#' @param x [\code{integer}] \cr Which features are active?
#' @param len [\code{integer(1)}] \cr Length of the feature vector.
#' @param out [\code{character(1)}] \cr Format of the output. Can be a vector or a matrix.
#' @return [\code{matrix(1, len)}] A one-row matrix with \code{len} columns with every entry 0 
#'   except the columns specified by \code{x}.
#' @export
#' @examples 
#' makeNHot(c(1, 3), 5)
#' makeNHot(c(1, 3), 5, out = "vector")
makeNHot = function(x, len, out = "matrix") {
  checkmate::assertIntegerish(x, max.len = len)
  checkmate::assertInt(len)
  if (out == "matrix") {
    m = matrix(rep(0, len), nrow = 1)
    m[1, x] = 1
  } else {
    m = rep(0, len)
    m[x] = 1
  }
  m
}

