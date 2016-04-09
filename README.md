# Arrowized functional state machines

## State machine

The intuitive way to understand the state machine is that it have four parts, state, storage, input and output. Each state has its own transition function which takes the storage, the input and updates the storage and the state and gives the output.

The abstract way is thinking the state machine as the stateful function. 

Our plan is using the stateful function as the interface, but users can build state machines in an intuitive way.

## Basic Concepts

The ```SM a b``` type denotes stateful functions from ```a``` to ```b```.

(>>>) :: SM a b -> SM b c -> SM a c

(&&&) :: SM a b -> SM a c -> SM a (b, c)

exec :: SM a b -> [a] -> (SM a b, [b])

From the theoretical perspective, this model is a simplified version of FRP, but adding states on functions directly.

From the engineering perspective, the other difference from AFRP is that we provide the constructor to use the transition function ```trans::s->a->(SM a b, b)``` to build ```SM a b``` directly.

### Simplifed model

In functional reactive programming(FRP), the key concepts are the signal, ```Signal a :: Time -> a```, and the signal function from signal to signal, ```SF a b :: Signal a -> Signal b```.

The model of FRP is beautiful, but one diffcult thing is that the signal is continuous function, and our computers are discrete systems. 

However, what if we do not care about time, and only focus on the sequence of input. There is reason to believe that computational tasks usually do not care about time. For example, the parsing process. So ```[a]``` and ```[Event a]``` are the only things we expected.

### Stateful function(Storage)

Usually, the state can be abstracted to the summary of input history. With ArrowLoop class, we can implement a stateful function in FPR.
If we want to get a function ```SF a b``` with state ```c```. We implement a function ```SF (a, c) (b, c)```, and use
```loop :: (SF (a, c) (b, c))->SF a b``` to get the stateful function ```SF a b```.

But I prefer to think the pure function as the stateful function with ```()```, because the stateful function gives us a more natural way to represent the control flow. Also, it give us the ability to switch the function itself based on different inputs.

## Implementation

The key idea is using the GADTs extension to hide the state(storage) type. If we do not use the GADTs extension, then ```SM a b``` will become ```SM s a b``` where ```s``` denotes the state type.

## To-Do
  * Basic state machines
  * Event
  * More high order functions
  * Another DSL to build transition functions?
