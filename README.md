
<img src="reinforcelearn.png" width="150px" />

Reinforcement Learning in R.
----------------------------

------------------------------------------------------------------------

[![Travis-CI Build Status](https://travis-ci.org/markdumke/reinforcelearn.svg?branch=master)](https://travis-ci.org/markdumke/reinforcelearn) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/reinforcelearn)](https://cran.r-project.org/package=reinforcelearn) [![Coverage Status](https://img.shields.io/codecov/c/github/markdumke/reinforcelearn/master.svg?maxAge=600)](https://codecov.io/github/markdumke/reinforcelearn?branch=master)

### Installation

``` r
# install.packages("devtools")
devtools::install_github("markdumke/reinforcelearn")
```

------------------------------------------------------------------------

### Get started

Reinforcement Learning with the package `reinforcelearn` is as easy as

``` r
library(reinforcelearn)

# Create gridworld environment
env = windyGridworld()

# Solve environment using Sarsa
res = sarsa(env, n.episodes = 30)
print(res$steps)
#>  [1]  818 1807  727  460  410  128  369  159  332   33  221  148  329   96
#> [15]   89  270  122   68  177  185  179   92  117   46  225  116  190   90
#> [29]  137  184
```

------------------------------------------------------------------------

### Environments

With `makeEnvironment` you can create a reinforcement learning environment from a Markov Decision Process.

``` r
# Create environment from MDP.
P = array(0, c(2,2,2))
P[, , 1] = matrix(c(0.2, 0.8, 0, 1), 2, 2, byrow = TRUE)
P[, , 2] = matrix(c(0.1, 0.9, 0, 1), 2, 2, byrow = TRUE)
R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)  
env = makeEnvironment(transitions = P, rewards = R)
```

The environment is an `R6` class with a set of attributes and methods. You can interact with the environment via the `reset` and `step` method.

``` r
# Reset environment.
env$reset()
print(env)
#> Number of steps: 0 
#> State: 0 
#> Reward:  
#> Done: FALSE

# Take action 0.
env$step(0)
print(env)
#> Number of steps: 1 
#> State: 0 
#> Reward: 5 
#> Done: FALSE
```

You can also create an environment from [OpenAI Gym](https://gym.openai.com/). You need to install all dependencies listed [here](https://github.com/openai/gym-http-api). Then you can use an environment with the name.

``` r
# Create Gym environment.
# Note: There is a bug: The following line might return an error. 
# If so, repeat this line, then it should work.
m = makeEnvironment("MountainCar-v0")

m$reset()
# take random actions for 200 steps
for (i in 1:200) {
  action = sample(m$actions, 1)
  m$step(action)
}
m$close()
```

This should open a Python window showing the interaction with the environment.

For more details on how to create an environment have a look at the vignette: [How to create an environment?](markdumke.github.io/reinforcelearn/articles/environments.html)

------------------------------------------------------------------------

### Algorithms

After you created an environment you can use various reinforcement learning algorithms to sovle this environment. For example, for a tabular environment like gridworld you can use tabular Q-Learning to solve it and find the optimal action value function *Q*\*. You can set various parameters like the learning rate, the number of episodes, the discount factor or epsilon.

``` r
# Create the windy gridworld environment.
env = windyGridworld()
res = qlearning(env, n.episodes = 30)
print(res$steps)
#>  [1] 1005 1618  739  330  546   79  147  389  220  241   78  179  119  395
#> [15]  215   49  205   60  196   35  295   46   78  198   87   79  143  100
#> [29]  145  255

# Show value of each state.
print(matrix(round(apply(res$Q1, 1, max), 1), ncol = 10, byrow = TRUE))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,] -4.8 -5.0 -5.6 -6.3 -7.1 -7.5 -7.4 -6.8 -6.1  -5.2
#> [2,] -4.5 -4.6 -4.8 -5.2 -4.9 -4.3 -3.0 -3.9 -4.4  -4.3
#> [3,] -4.3 -4.2 -4.1 -4.0 -3.5 -2.0 -1.3 -2.3 -3.2  -3.5
#> [4,] -3.9 -3.7 -3.5 -3.3 -1.9 -0.9 -0.3  0.0 -2.4  -2.6
#> [5,] -3.4 -3.2 -2.8 -2.3 -1.1 -0.3  0.0 -0.3 -0.9  -1.8
#> [6,] -2.9 -2.7 -2.2 -1.6 -0.7  0.0  0.0  0.0 -0.7  -1.1
#> [7,] -2.7 -2.4 -1.8 -1.1  0.0  0.0  0.0  0.0 -0.3  -0.7
```

We can then get the optimal policy by taking the argmax over the action value function Q.

``` r
optimal.policy = max.col(res$Q1) - 1L
print(matrix(optimal.policy, ncol = 10, byrow = TRUE))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    3    2    0    3    0    0    1    1    1     3
#> [2,]    1    0    3    3    0    3    2    3    3     2
#> [3,]    3    3    1    3    3    3    0    2    1     3
#> [4,]    3    0    3    2    0    1    1    3    0     3
#> [5,]    2    2    1    2    1    0    1    3    0     0
#> [6,]    3    3    1    1    2    1    0    3    0     0
#> [7,]    2    1    2    1    0    2    1    0    0     1
```

For more details on algorithms have a look at the vignette: [How to solve an environment?](markdumke.github.io/reinforcelearn/articles/algorithms.html)

------------------------------------------------------------------------

### Value function approximation

When the state space is large or even continuous tabular solution methods cannot be applied. Then it is better to approximate the value function using a function approximator. We need to define a function, which preprocesses the state observation, so that the function approximator can work with it. Here is an example solving the mountain car problem using linear function approximation.

``` r
# Set up the Mountain Car problem
m = mountainCar()

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

preprocessState = function(state) {
  # scale state observation
  state = matrix(c(position.scale * state[1], velocity.scale * state[2]), ncol = 2)
  # get active tiles
  active.tiles = tiles(iht, 8, state)
  # return n hot vector with 1 at the position of each active tile
  makeNHot(active.tiles, max.size)
}

set.seed(123)
res = qlearning(m, fun.approx = "linear", 
  preprocessState = preprocessState, n.episodes = 20)
print(res$steps)
#>  [1] 1211  903  536  420  406  241  241  239  233  232  204  241  194  235
#> [15]  233  167  198  165  234  162
```

------------------------------------------------------------------------

### Vignettes

Also have a look at the vignettes for further examples.

-   [Introduction to reinforcelearn](markdumke.github.io/reinforcelearn/articles/introduction.html)
-   [How to create an environment?](markdumke.github.io/reinforcelearn/articles/environments.html)
-   [How to solve an environment?](markdumke.github.io/reinforcelearn/articles/algorithms.html)

------------------------------------------------------------------------

Logo is a modification of <https://www.r-project.org/logo/>.

Author: Markus Dumke

Date: "16 Oktober 2017"
