# AFSM
Arrowized functional state machines

## Basic Concepts

The ```SM a b``` type denotes stateful functions from ```a``` to ```b```.

(>>>) :: SM a b -> SM b c -> SM a c

(&&&) :: SM a b -> SM a c -> SM a (b, c)

exec :: SM a b -> [a] -> (SM a b, [b])

From the theoretical perspective, this model is a simplified version of FRP, but adding states on functions directly.

From the engineering perspective, the other different from AFRP is that we provide constructors to use the transition function ```trans::r->a->(SM a b, b)``` to build ```SM a b``` directly. 

### Simplifed model

In functional reactive programming(FRP), the key concepts are the signal, ```Signal a :: Time -> a```, and the signal function from signal to signal, ```SF a b :: Signal a -> Signal b```.

The model is beautiful, but one diffcult thing of FPR is that the signal is continuous function, and our computers are discrete systems. 

However, what if we do not care about time, and only focus on the sequence of input. There is reason to believe that computational tasks usually do not care about time. For example, the parsing process. So ```[a]``` and ```[Event a]``` are the only things we expected.

### State

Usually, the state can be abstracted to the summary of input history. With ArrowLoop class, we can implement a stateful function in FPR.
If we want to get a function ```SF a b``` with state ```c```. We implement a function ```SF (a, c) (b, c)```, and use
```loop :: (SF (a, c) (b, c))->SF a b``` to get the stateful function ```SF a b```.

But I prefer to think the pure function as the stateful function with ```()```, because the stateful function gives us a more natural way to represent the control flow. Also, it give us the ability to switch the function itself based on different inputs.

## Implementation

The key idea is using the GADTs extension to hide the state type. If we do not use the GADTs extension, then ```SM a b``` will become ```SM r a b``` where ```r``` denotes the state type or the resource type.

## To-Do
  * Basic state machine
  * Event
  * More high order functions
