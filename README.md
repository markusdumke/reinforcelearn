<div align="center">
  <img width="50%" src="ReinforceLogo2.png"><br><br>
</div>

-----------------

Reinforcement Learning in R. Work in progress!

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
transitions = gridworld$transitions
rewards = gridworld$rewards
env = makeEnvironment(transition.array = transitions, 
  reward.matrix = rewards)

# Solve environment using Value Iteration
iterateValue(env)
```

### Create an environment

With `makeEnvironment` you can create a reinforcement learning environment, either from [OpenAI Gym](https://gym.openai.com/) or from the state transition matrix and reward matrix of a Markov Decision Process. The `makeEnvironment` will create an `R6 class`, from which you can access attributes about state and action space and use the `reset` and `step` methods to sample experiences from the environment.

To use a Gym environment you need to have the prerequisites installed: [gym-http-api](https://github.com/openai/gym-http-api)

```r
# Create an OpenAI Gym environment.
MountainCar = makeEnvironment("MountainCar-v0")

MountainCar$reset() # returns initial state observation
# take random actions for 200 steps
for (i in 1:200) {
  action = sample(MountainCar$actions, 1)
  MountainCar$step(action)
}
MountainCar$close()

# You can also create an environment from a MDP
grid = makeEnvironment(transition.array = windyGridworld$transitions,
  reward.matrix = windyGridworld$rewards,
  initial.state = 30L)
```

Environments created with `makeEnvironment` always have an initialization function to create a new instance of the environment, a reset function, which returns an initial state observation and a step function, which takes a step in the environment given an action returning the next state observation, reward and if the episode is finished.

### Run a reinforcement learning algorithm

After you created an environment you can use various reinforcement learning algorithms. For example, for a tabular environment like  gridworld you can use tabular Q-Learning to solve it and find the optimal action value function Q*. You can set various parameters like the learning rate, the number of episodes, the discount factor or epsilon, the ratio of random actions sampled by an epsilon-greedy behaviour policy.

```r
res = qlearning(grid, n.episodes = 1000, seed = 123)
# reshape action value function Q
print(matrix(apply(res$Q, 1, max), ncol = 10, byrow = TRUE))
```

We can then define the optimal policy by taking the argmax over the action value function Q.

```r
optimal.policy = max.col(res$Q)
print(matrix(optimal.policy, ncol = 10, byrow = TRUE))
```

### Use function approximation for complex environments



Also have a look at the vignettes for further examples.

Logo is modification of https://www.r-project.org/logo/.
