Agent
================
Markus Dumke
Mon Dec 04 11:24:47 2017

``` r
# Switch to branch "markus"
devtools::load_all()
library(keras)
set.seed(1)

env = GridworldEnvironment$new(shape = c(4, 4), goal.states = 0, initial.state = 15)
```

------------------------------------------------------------------------

### Agent Components

-   Representation of value function (optional)
-   Policy (e.g. epsilon-greedy)
-   Learner (how to learn, e.g. Q-Learning)

------------------------------------------------------------------------

### State Preprocessor (user-defined function)

-   transform the state observation so that the agent can learn on this

``` r
preprocessState = function(state) {
  reinforcelearn::nHot(state + 1, env$n.states)
}

(s = env$reset())
```

    #> [1] 15

``` r
(phi = preprocessState(s))
```

    #>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
    #> [1,]    0    0    0    0    0    0    0    0    0     0     0     0     0
    #>      [,14] [,15] [,16]
    #> [1,]     0     0     1

------------------------------------------------------------------------

### ActionValueNetwork

-   value function can be represented as a table or neural network
-   R6 class to represent value function
-   user defines keras or mxnet network

-   `predictQ(state)`
    -   predict action values given state
-   `train(state, target)`
    -   train model on state-target pair
    -   calling `keras::fit()`

``` r
model.keras = keras_model_sequential()
model.keras %>% layer_dense(units = 4, activation = "linear", input_shape = c(16),
  use_bias = FALSE, kernel_initializer = "zeros")
keras::compile(model.keras, loss = "mae", optimizer = keras::optimizer_sgd(lr = 0.4))

action.network = ActionValueNetwork$new(model.keras, preprocessState)
action.network$model
```

    #> Model
    #> ___________________________________________________________________________
    #> Layer (type)                     Output Shape                  Param #     
    #> ===========================================================================
    #> dense_1 (Dense)                  (None, 4)                     64          
    #> ===========================================================================
    #> Total params: 64.0
    #> Trainable params: 64
    #> Non-trainable params: 0.0
    #> ___________________________________________________________________________

``` r
(Q = action.network$predictQ(phi))
```

    #>      [,1] [,2] [,3] [,4]
    #> [1,]    0    0    0    0

``` r
# action.network$train(state, target)
```

------------------------------------------------------------------------

### ActionValueTable

``` r
action.table = ActionValueTable$new(env$n.states, env$n.actions, step.size = 1)
action.table$Q
```

    #>       [,1] [,2] [,3] [,4]
    #>  [1,]    0    0    0    0
    #>  [2,]    0    0    0    0
    #>  [3,]    0    0    0    0
    #>  [4,]    0    0    0    0
    #>  [5,]    0    0    0    0
    #>  [6,]    0    0    0    0
    #>  [7,]    0    0    0    0
    #>  [8,]    0    0    0    0
    #>  [9,]    0    0    0    0
    #> [10,]    0    0    0    0
    #> [11,]    0    0    0    0
    #> [12,]    0    0    0    0
    #> [13,]    0    0    0    0
    #> [14,]    0    0    0    0
    #> [15,]    0    0    0    0
    #> [16,]    0    0    0    0

``` r
(Q = action.table$predictQ(s))
```

    #> [1] 0 0 0 0

``` r
action.table$train(state = 15, target = matrix(-1, 4))
action.table$Q
```

    #>       [,1] [,2] [,3] [,4]
    #>  [1,]    0    0    0    0
    #>  [2,]    0    0    0    0
    #>  [3,]    0    0    0    0
    #>  [4,]    0    0    0    0
    #>  [5,]    0    0    0    0
    #>  [6,]    0    0    0    0
    #>  [7,]    0    0    0    0
    #>  [8,]    0    0    0    0
    #>  [9,]    0    0    0    0
    #> [10,]    0    0    0    0
    #> [11,]    0    0    0    0
    #> [12,]    0    0    0    0
    #> [13,]    0    0    0    0
    #> [14,]    0    0    0    0
    #> [15,]    0    0    0    0
    #> [16,]   -1   -1   -1   -1

------------------------------------------------------------------------

### Policy

-   R6 class to represent the policy
-   `getActionProbs(Q)`
    -   return probabilities for each action given value function
-   `sampleAction(probs)`
    -   sample action from policy probabilities

``` r
policy = RandomPolicy$new()
(probs = policy$getActionProbs(Q))
```

    #>      [,1] [,2] [,3] [,4]
    #> [1,] 0.25 0.25 0.25 0.25

``` r
policy = EpsilonGreedyPolicy$new(epsilon = 0.1)
policy$epsilon
```

    #> [1] 0.1

``` r
(probs = policy$getActionProbs(Q))
```

    #>       [,1]  [,2]  [,3]  [,4]
    #> [1,] 0.925 0.025 0.025 0.025

``` r
(action = policy$sampleAction(probs))
```

    #> [1] 0

------------------------------------------------------------------------

### Learner

-   e.g. Q-Learning, Sarsa, ...
-   `getTarget(reward, discount, action.values)`
    -   get target used for training (TD target)

``` r
learner = QLearning$new()
learner$getTarget(reward = 1.5, discount = 1, action.values = c(0, 2, 1))
```

    #> [1] 3.5

------------------------------------------------------------------------

### Agent

-   `act(state)`
    -   returns action sampled from policy
-   `learn(state, action , new.state, reward)`
    -   update value function / policy parameters

``` r
agent = Agent$new(learner, action.table, policy)
agent$act(state = 1)
```

    #> [1] 0

``` r
# agent$learn()
```

------------------------------------------------------------------------

### Interaction

-   function for interaction
-   calls `agent$act()`, `environment$step()` and `agent$learn()` in a loop
-   learning is optional
-   allows visualization and single step mode
-   modifies agent and environment
-   returns number of steps, returns per episode
-   logging to file possible

``` r
interaction(env, agent, n.steps = 1000)
```

Get learned action value function

``` r
(Q = agent$action.value$Q)
```

    #>       [,1] [,2] [,3] [,4]
    #>  [1,]    0    0    0    0
    #>  [2,]   -1   -1   -1   -3
    #>  [3,]   -2   -2   -2   -3
    #>  [4,]   -3   -3   -3   -4
    #>  [5,]   -2   -3   -1   -3
    #>  [6,]   -2   -4   -2   -2
    #>  [7,]   -3   -3   -3   -3
    #>  [8,]   -4   -4   -4   -5
    #>  [9,]   -3   -4   -2   -4
    #> [10,]   -3   -3   -3   -5
    #> [11,]   -4   -5   -4   -4
    #> [12,]   -5   -5   -5   -6
    #> [13,]   -4   -5   -3   -4
    #> [14,]   -4   -6   -4   -5
    #> [15,]   -5   -7   -5   -6
    #> [16,]   -6   -7   -6   -7

Get state value function

``` r
matrix(round(apply(Q, 1, max), 2), ncol = 4)
```

    #>      [,1] [,2] [,3] [,4]
    #> [1,]    0   -1   -2   -3
    #> [2,]   -1   -2   -3   -4
    #> [3,]   -2   -3   -4   -5
    #> [4,]   -3   -4   -5   -6

Run learned policy

``` r
env$reset()
```

    #> [1] 15

``` r
env$visualize()
```

    #>  - - - - 
    #>  - - - - 
    #>  - - - - 
    #>  - - - o 
    #> 

``` r
# act greedily
agent$policy$epsilon = 0 # caveat: may run forever for other seed
for (i in 1:6) {
  ## Comment in next line to step through environment
  # invisible(readline(prompt = "Press [enter] to take the next action"))
  interaction(env, agent, 1, learn = FALSE, visualize = TRUE)
}
```

    #>  - - - - 
    #>  - - - - 
    #>  - - - - 
    #>  - - o - 
    #>  
    #>  - - - - 
    #>  - - - - 
    #>  - - - - 
    #>  - o - - 
    #>  
    #>  - - - - 
    #>  - - - - 
    #>  - - - - 
    #>  o - - - 
    #>  
    #>  - - - - 
    #>  - - - - 
    #>  o - - - 
    #>  - - - - 
    #>  
    #>  - - - - 
    #>  o - - - 
    #>  - - - - 
    #>  - - - - 
    #>  
    #>  o - - - 
    #>  - - - - 
    #>  - - - - 
    #>  - - - - 
    #>
