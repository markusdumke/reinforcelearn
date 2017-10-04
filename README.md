<div align="center">
  <img width="50%" src="inst/ReinforceLogo.png"><br><br>
</div>

-----------------

R Package for Reinforcement Learning. Work in progress! 

### Installation

```r
# install.packages("devtools")
devtools::install_github("markdumke/reinforcelearn")

# Loading the package
library(reinforcelearn)
```

### Get started

Reinforcement Learning with the package `reinforcelearn` is as easy as
```r
# Create gridworld environment
env = windyGridworld()

# Solve environment using Sarsa
sarsa(env)
```

### Create an environment

With `makeEnvironment` you can create a reinforcement learning environment, either from [OpenAI Gym](https://gym.openai.com/) or from the state transition matrix and reward matrix of a Markov Decision Process. The `makeEnvironment` will create an `R6 class`, from which you can access attributes about state and action space and use the `reset` and `step` methods to sample experiences from the environment.

To use a Gym environment you need to have the prerequisites installed: [gym-http-api](https://github.com/openai/gym-http-api)

```r
# Create Gym environment.
# Note: There is a bug: The following line might return an error. If so, repeat this line, then it should work.
MountainCar = makeEnvironment("MountainCar-v0")

MountainCar$reset()
# take random actions for 200 steps
for (i in 1:200) {
  action = sample(MountainCar$actions, 1)
  MountainCar$step(action)
}
MountainCar$close()

# Create environment from MDP
P = array(0, c(2,2,2))
P[, , 1] = matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow = TRUE)
P[, , 2] = matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow = TRUE)
R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)  
env = makeEnvironment(transitions = P, rewards = R)
```

Environments created with `makeEnvironment` always have a reset function, which returns an initial state observation and a step function, which takes an action as input and returns the next state observation, reward and if the episode is finished.

### Run a reinforcement learning algorithm

After you created an environment you can use various reinforcement learning algorithms. For example, for a tabular environment like  gridworld you can use tabular Q-Learning to solve it and find the optimal action value function Q*. You can set various parameters like the learning rate, the number of episodes, the discount factor or epsilon, the probability to sample a random action.

```r
env = windyGridworld()
res = qlearning(env, n.episodes = 200)
# reshape action value function Q
print(matrix(apply(res$Q1, 1, max), ncol = 10, byrow = TRUE))
```

We can then get the optimal policy by taking the argmax over the action value function Q.

```r
optimal.policy = max.col(res$Q1)
print(matrix(optimal.policy, ncol = 10, byrow = TRUE))
```

### Use function approximation for complex environments

When the state space is large or even continuous tabular solution methods cannot be applied. Then it is better to approximate the value function using a function approximator.
We need to define a function, which preprocesses the state observation, so that the function approximator can work with it.
Here is an example solving Mountain Car using linear function approximation. 

```r
# Solve the Mountain Car problem using linear function approximation
m = MountainCar()

# Define preprocessing function (here grid tiling)
n.tilings = 8
max.size = 4096
iht = IHT(max.size)

position.max = m$state.space.bounds[[1]][2]
position.min = m$state.space.bounds[[1]][1]
velocity.max = m$state.space.bounds[[2]][2]
velocity.min = m$state.space.bounds[[2]][1]
position.scale = n.tilings / (position.max - position.min)
velocity.scale = n.tilings / (velocity.max - velocity.min)

# scales state observation
preprocessState = function(state) {
  # scale state observation
  state = matrix(c(position.scale * state[1], velocity.scale * state[2]), ncol = 2)
  # get active tiles
  active.tiles = tiles(iht, 8, state)
  # return n hot vector with 1 at the position of each active tile
  makeNHot(active.tiles, max.size)
}

set.seed(123)
qSigma(m, value.function = "linear", preprocessState = preprocessState, 
  n.episodes = 50)
```

Also have a look at the vignettes for further examples.

---

Logo is a modification of https://www.r-project.org/logo/.
