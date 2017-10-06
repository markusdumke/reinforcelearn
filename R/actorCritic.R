#' TD Actor Critic
#' @export
#' @examples 
#' # Variant of cliff walking
#' library(reinforcelearn)
#' rewardFun = function(state, action, n.state) {
#'   if (n.state %in% 37:46) {
#'     return(- 100)
#'   } else {
#'     return(- 1)
#'   }
#' }
#' env = makeGridworld(shape = c(4, 12), goal.states = 47,
#'   cliff.states = 37:46, reward.step = - 1, reward.cliff = - 100,
#'   cliff.transition.done = TRUE, initial.state = 36, sampleReward = rewardFun)
#' 
#' tdActorCritic(env, n.episodes = 300)
#' 
#' # Mountain Car
#' m = MountainCar()
#' 
#' # Define preprocessing function (we use grid tiling)
#' n.tilings = 8
#' max.size = 4096
#' iht = IHT(max.size)
#' 
#' position.max = m$state.space.bounds[[1]][2]
#' position.min = m$state.space.bounds[[1]][1]
#' velocity.max = m$state.space.bounds[[2]][2]
#' velocity.min = m$state.space.bounds[[2]][1]
#' position.scale = n.tilings / (position.max - position.min)
#' velocity.scale = n.tilings / (velocity.max - velocity.min)
#' 
#' # Scale state first, then get active tiles and return n hot vector
#' preprocessState = function(state) {
#'   state = c(position.scale * state[1], velocity.scale * state[2])
#'   active.tiles = tiles(iht, 8, state)
#'   makeNHot(active.tiles, max.size, out = "vector")
#' }
#' 
#' # Linear function approximation and softmax policy
#' tdActorCritic(m, fun.approx = "linear", preprocessState = preprocessState)
#' 
#' #----------------
#' # Mountain Car with continuous action space
#' m2 = MountainCar(action.space = "Continuous")
#' 
#' # Linear function approximation and gaussian policy
#' tdActorCritic(m, fun.approx = "linear", policy = "gaussian", 
#'   preprocessState = preprocessState)
#' 
tdActorCritic = function(envir, fun.approx = "table", policy = "softmax", preprocessState = identity,
  n.episodes = 100, discount = 1, alpha = 0.01, beta = 0.1, lambda = 0) {
  
  steps = rep(0, n.episodes)
  returns = rep(0, n.episodes)
  
  if (fun.approx == "table" & policy == "softmax") {
    policy = matrix(1 / envir$n.actions, nrow = envir$n.states, ncol = envir$n.actions)
    h = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
    v = rep(0, envir$n.states)
    
    for (i in seq_len(n.episodes)) {
      envir$reset()
      e.actor = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
      e.critic = rep(0, envir$n.states)
      j = 1
      while(envir$done == FALSE) {
        s = preprocessState(envir$state)
        a = sampleActionFromPolicy(policy[s + 1, ])
        envir$step(a)
        r = envir$reward
        returns[i] = returns[i] + discount^(envir$n.steps - 1) * r
        s.n = preprocessState(envir$state)
        
        delta = r + discount * v[s.n + 1] - v[s + 1]
        if (envir$done) {
          delta = r - v[s + 1]
        }
        e.critic[s + 1] = e.critic[s + 1] + j
        e.actor[s + 1, ] = e.actor[s + 1, ] + j * (makeNHot(a + 1, envir$n.actions) - policy[s + 1, ])
        
        v = v + beta * delta * e.critic
        h = h + alpha * j * delta * e.actor
        
        e.critic = discount * lambda * e.critic
        e.actor = discount * lambda * e.actor
        
        policy = softmax(h)
        j = discount * j
        
        if (envir$done) {
          print(paste("Episode", i, "finished after", envir$n.steps, "steps."))
          steps[i] = envir$n.steps
          break
        }
      }
    }
    return(list(policy = policy, v = v, steps = steps, returns = returns))
  }
  
  if (fun.approx == "linear" & policy == "softmax") {
    envir$reset()
    n.weights = length(preprocessState(envir$state))
    theta = matrix(0, nrow = n.weights, ncol = envir$n.actions) # weights actor
    w = rep(0, n.weights) # different number of weights for actor and critic? # weights critic
    
    predictPolicy = function(s) {
      h = s %*% theta
      policy = softmax(h)
      return(policy)
    }
    
    predictV = function(s) {
      c(s %*% w)
    }
    
    for (i in seq_len(n.episodes)) {
      envir$reset()
      e.actor = matrix(0, nrow = n.weights, ncol = envir$n.actions)
      e.critic = rep(0, n.weights)
      j = 1
      while(envir$done == FALSE) {
        s = preprocessState(envir$state)
        policy = predictPolicy(s)
        action = sampleActionFromPolicy(policy)
        envir$step(action)
        r = envir$reward
        returns[i] = returns[i] + discount^(envir$n.steps - 1) * r
        s.n = preprocessState(envir$state)
        v = predictV(s)
        v.n = predictV(s.n)
        
        delta = r + discount * v.n - v
        if (envir$done) {
          delta = r - v
        }
        e.critic = e.critic + j * s
        s2 = matrix(0, nrow = length(s), ncol = envir$n.actions)
        s2[, action + 1] = s
        for (k in seq_len(envir$n.actions)) {
          e.actor[, k] = e.actor[, k] + j * (s2[, k] - policy[k] * s)
        }
        
        w = w + beta * delta * e.critic
        theta = theta + alpha * j * delta * e.actor
        
        e.critic = discount * lambda * e.critic
        e.actor = discount * lambda * e.actor
        
        j = discount * j
        
        if (envir$done) {
          print(paste("Episode", i, "finished after", envir$n.steps, "steps."))
          steps[i] = envir$n.steps
          break
        }
      }
    }
    return(list(policy = theta, v = w, steps = steps, returns = returns))
  }
  
  if (fun.approx == "linear" & policy == "gaussian") {
    envir$reset()
    n.weights = length(preprocessState(envir$state))
    theta.mu = rep(0, n.weights)
    theta.sigma = rep(0, n.weights)
    w = rep(0, n.weights)
    mu = 0
    sigma = 1
    
    predictV = function(s) {
      c(s %*% w)
    }
    
    for (i in seq_len(n.episodes)) {
      envir$reset()
      e.actor.mu = rep(0, n.weights)
      e.actor.sigma = rep(0, n.weights)
      e.critic = rep(0, n.weights)
      j = 1
      while(envir$done == FALSE) {
        s = preprocessState(envir$state)
        mu = c(theta.mu %*% s)
        sigma = exp(c(theta.sigma %*% s))
        action = rnorm(1, mean = mu, sd = sigma)
        envir$step(action)
        r = envir$reward
        returns[i] = returns[i] + discount^(envir$n.steps - 1) * r
        s.n = preprocessState(envir$state)
        v = predictV(s)
        v.n = predictV(s.n)
        
        delta = r + discount * v.n - v
        if (envir$done) {
          delta = r - v
        }
        e.critic = e.critic + j * s
        e.actor.mu = e.actor.mu + j * ((action - mu) * s / sigma^2)
        e.actor.sigma = e.actor.sigma + j * (- s + (action - mu)^2 / sigma^2 * s)
        
        w = w + beta * delta * e.critic
        theta.mu = theta.mu + alpha * j * delta * e.actor.mu
        theta.sigma = theta.sigma + alpha * j * delta * e.actor.sigma
        
        e.critic = discount * lambda * e.critic
        e.actor.mu = discount * lambda * e.actor.mu
        e.actor.sigma = discount * lambda * e.actor.sigma
        
        j = discount * j
        
        if (envir$done) {
          print(paste("Episode", i, "finished after", envir$n.steps, "steps."))
          steps[i] = envir$n.steps
          break
        }
      }
    }
    return(list(policy = list(theta.mu, theta.sigma), v = w, steps = steps, returns = returns))
  }
}

softmax = function(x) {
  exp(x) / rowSums(exp(x))
}
