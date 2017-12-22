
Reinforcement Learning in R <img src="man/figures/logo.png" align="right" height="36"/>
=======================================================================================

[![Travis-CI Build Status](https://travis-ci.org/markusdumke/reinforcelearn.svg?branch=master)](https://travis-ci.org/markusdumke/reinforcelearn) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/reinforcelearn)](https://cran.r-project.org/package=reinforcelearn) [![Coverage Status](https://img.shields.io/codecov/c/github/markusdumke/reinforcelearn/master.svg?maxAge=600)](https://codecov.io/github/markusdumke/reinforcelearn?branch=master)

### Documentation

[Website](https://markusdumke.github.io/reinforcelearn)

------------------------------------------------------------------------

### Installation

``` r
# Install from CRAN.
install.packages("reinforcelearn")

# Install development version from github.
devtools::install_github("markusdumke/reinforcelearn")
```

------------------------------------------------------------------------

### Get started

Reinforcement Learning with the package `reinforcelearn` is as easy as

``` r
library(reinforcelearn)

env = makeEnvironment("windy.gridworld")
agent = makeAgent("softmax", "table", "qlearning")

# Run interaction for 10 episodes.
interact(env, agent, n.episodes = 10L)
#> $returns
#>  [1] -1358 -1604  -465 -1041 -1784  -290  -620  -520   -64  -632
#> 
#> $steps
#>  [1] 1358 1604  465 1041 1784  290  620  520   64  632
```

------------------------------------------------------------------------

### Environments

With `makeEnvironment` you can create reinforcement learning environments.

``` r
# Create environment.
step = function(self, action) {
  state = list(mean = action + rnorm(1), sd = runif(1))
  reward = rnorm(1, state[[1]], state[[2]])
  done = FALSE
  list(state, reward, done)
}

reset = function(self) {
  state = list(mean = 0, sd = 1)
  state
}

env = makeEnvironment("custom", step = step, reset = reset)
```

The environment is an `R6` class with a set of attributes and methods. You can interact with the environment via the `reset` and `step` method.

``` r
# Reset environment.
env$reset()
#> $mean
#> [1] 0
#> 
#> $sd
#> [1] 1

# Take action.
env$step(100)
#> $state
#> $state$mean
#> [1] 100.0457
#> 
#> $state$sd
#> [1] 0.225038
#> 
#> 
#> $reward
#> [1] 100.1838
#> 
#> $done
#> [1] FALSE
```

There are some predefined environment classes, e.g. `MDPEnvironment`, which allows you to create a Markov Decision Process by passing on state transition array and reward matrix, or `GymEnvironment`, where you can use toy problems from [OpenAI Gym](https://gym.openai.com/).

``` r
# Create a gym environment.
# Make sure you have Python, gym and reticulate installed.
env = makeEnvironment("gym", gym.name = "MountainCar-v0")

# Take random actions for 200 steps.
env$reset()
for (i in 1:200) {
  action = sample(0:2, 1)
  env$step(action)
  env$visualize()
}
env$close()
```

This should open a window showing a graphical visualization of the environment during interaction.

For more details on how to create an environment have a look at the vignette: [Environments](https://markusdumke.github.io/reinforcelearn/articles/environments.html)

------------------------------------------------------------------------

### Agents

With `makeAgent` you can set up a reinforcement learning agent to solve the environment, i.e. to find the best action in each time step.

The first step is to set up the policy, which defines which action to choose. For example we could use a uniform random policy.

``` r
# Create the environment.
env = makeEnvironment("windy.gridworld")

# Create agent with uniform random policy.
policy = makePolicy("random")
agent = makeAgent(policy)

# Run interaction for 10 steps.
interact(env, agent, n.steps = 10L)
#> $returns
#> numeric(0)
#> 
#> $steps
#> integer(0)
```

In this scenario the agent chooses all actions with equal probability and will not learn anything from the interaction. Usually we want the agent to be able to learn something. Value-based algorithms learn a value function from interaction with the environment and adjust the policy according to the value function. For example we could set up Q-Learning with a softmax policy.

``` r
# Create the environment.
env = makeEnvironment("windy.gridworld")

# Create qlearning agent with softmax policy and tabular value function.
policy = makePolicy("softmax")
values = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
algorithm = makeAlgorithm("qlearning")
agent = makeAgent(policy, values, algorithm)

# Run interaction for 10 steps.
interact(env, agent, n.episodes = 10L)
#> $returns
#>  [1] -2206  -355 -2396  -985  -912 -1027  -217  -326  -483  -818
#> 
#> $steps
#>  [1] 2206  355 2396  985  912 1027  217  326  483  818
```

------------------------------------------------------------------------

### Vignettes

Also have a look at the vignettes for further examples.

-   [Environments](https://markusdumke.github.io/reinforcelearn/articles/environments.html)
-   [Agents](https://markusdumke.github.io/reinforcelearn/articles/agents.html)

------------------------------------------------------------------------

Logo is a modification of <https://www.r-project.org/logo/>.
