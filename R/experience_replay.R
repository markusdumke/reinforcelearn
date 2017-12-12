#' @export
ExperienceReplay = R6::R6Class("ExperienceReplay",
  public = list(
    memory = NULL,
    size = NULL,
    index = 0L,

    # fixme allow growing replay memory?
    initialize = function(size) {
      self$size = size
      self$memory = vector("list", length = self$size)
    },

    # # initialize following policy
    # initializeMemory = function(env, policy) {
    #   for (i in seq_len(self$size)) {
    #     action = policy$sampleAction()
    #     env$step(action)
    #     data = list(state = preprocessState(envir$previous.state), action = action,
    #       reward = envir$reward, next.state = preprocessState(envir$state))
    #   }
    # },

    observe = function(state, action, reward, next.state) {
      self$index = self$index + 1L
      index = self$getReplacementIndex()
      obs = self$getReplayObservation(state, action, reward, next.state)
      self$add(obs, index)
    },

    getReplayObservation = function(state, action, reward, next.state) {
      list(state = state, action = action, reward = reward, next.state = next.state)
    },

    # e.g. oldest entry
    getReplacementIndex = function() {
      if (self$index > self$size) {
        self$index = 1L
      }
      self$index
    },

    add = function(observation, index) {
      self$memory[[index]] = observation
    },

    isFull = function() {
      # maybe it is enough to check the last entry
      full = !(any(purrr::map_lgl(self$memory, is.null)))
      full
    },

    extract = function(batch, member, fun = lapply) {
      states = fun(batch, "[[", member)
      states
    },

    sampleBatch = function(memory = self$memory, batch.size) {
      indices = self$getIndices(length(memory), batch.size)
      batch = memory[indices]
      purrr::transpose(batch)
    },

    getIndices = function(memory.size, batch.size) {
      indices = sample(seq_len(memory.size), size = batch.size)
      indices
    }
  )
)

# ideas: maybe replay memory in future not list but hash table / dictionary etc
# fixme allow dynamic change of replay memory length
# store preprocessed state?
