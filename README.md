
<img src="inst/reinforcelearn.png" width="150px" />  
Reinforcement Learning in R.

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
#>  [1] 1244 1434  600  316  573   70  293  360  148  256  177  220  117  243
#> [15]   34   61  239  190  177  181  102  174   82   52  104   51  214  133
#> [29]  170  126
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
#> State: 1 
#> Reward: 5 
#> Done: TRUE
```

You can also create an environment from [OpenAI Gym](https://gym.openai.com/). You need to install all dependencies listed [here](https://github.com/openai/gym-http-api). Then you can use an environment with the name.

``` r
# Create Gym environment.
# Note: There is a bug: The following line might return an error. 
# If so, repeat this line, then it should work.
MountainCar = makeEnvironment("MountainCar-v0")

MountainCar$reset()
# take random actions for 200 steps
for (i in 1:200) {
  action = sample(MountainCar$actions, 1)
  MountainCar$step(action)
}
MountainCar$close()
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
#>  [1]  950 1446  912  336  496  219  185  219  236  239  352  147   64  129
#> [15]  219  243  135  134   49  343  185   68  108   84  128  159   70   87
#> [29]   21  108

# Show value of each state.
print(matrix(round(apply(res$Q1, 1, max), 1), ncol = 10, byrow = TRUE))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,] -4.6 -4.9 -5.4 -6.2 -6.9 -7.4 -7.3 -6.7 -5.9  -5.1
#> [2,] -4.5 -4.5 -4.7 -5.0 -4.9 -4.0 -2.9 -3.9 -4.2  -4.2
#> [3,] -4.2 -4.1 -4.0 -4.1 -3.4 -1.9 -1.3 -2.2 -3.2  -3.4
#> [4,] -3.9 -3.6 -3.4 -3.1 -2.0 -0.8 -0.3  0.0 -2.1  -2.5
#> [5,] -3.4 -3.1 -2.7 -2.3 -1.1 -0.3  0.0 -0.3 -0.9  -1.7
#> [6,] -2.9 -2.6 -2.2 -1.6 -0.6  0.0  0.0  0.0 -0.8  -1.2
#> [7,] -2.5 -2.3 -1.8 -1.1  0.0  0.0  0.0  0.0 -0.3  -0.7
```

We can then get the optimal policy by taking the argmax over the action value function Q.

``` r
optimal.policy = max.col(res$Q1) - 1L
print(matrix(optimal.policy, ncol = 10, byrow = TRUE))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    1    1    0    0    0    0    1    1    1     2
#> [2,]    2    1    0    0    3    0    2    0    3     3
#> [3,]    2    2    3    3    3    3    1    1    0     2
#> [4,]    1    0    3    2    0    2    0    2    3     0
#> [5,]    3    3    3    2    3    0    1    0    0     0
#> [6,]    3    0    1    1    1    2    1    2    0     3
#> [7,]    3    3    1    3    3    1    2    3    3     0
```

For more details on algorithms have a look at the vignette: [How to solve an environment?](markdumke.github.io/reinforcelearn/articles/algorithms.html)

------------------------------------------------------------------------

### Value function approximation

When the state space is large or even continuous tabular solution methods cannot be applied. Then it is better to approximate the value function using a function approximator. We need to define a function, which preprocesses the state observation, so that the function approximator can work with it. Here is an example solving the mountain car problem using linear function approximation.

``` r
# Set up the Mountain Car problem
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

Date: "15 Oktober 2017"
