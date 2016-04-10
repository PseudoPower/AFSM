# Arrowized functional state machines

## Introduction

The intuitive way to understand the state machine is that it have four parts, state, storage, input and output. Each state has its own transition function which takes the storage and the input, updates the storage and the state, and gives the output.

The abstract way is thinking the state machine as the stateful function.

Now, let us put these two ways together. Our plan is using the stateful function as the interface, but users can build state machines in an intuitive way.

## Basic Concepts

The ```SM a b``` type denotes stateful functions from ```a``` to ```b```.
```
data SM a b
--
--    a  /--------\  b
--  >--->| SM a b |>--->
--       \--------/

(>>>) :: SM a b -> SM b c -> SM a c
--
--    a  /--------\  b  /--------\  c
--  >--->| SM a b |>--->| SM b c |>--->
--       \--------/     \--------/

(&&&) :: SM a b -> SM a c -> SM a (b, c)
--
--           /--------\  b
--      /--->| SM a b |>---\
--    a |    \--------/    | (b,c)
--  >---|                  |------->
--      |    /--------\  c |
--      \--->| SM a c |>---/
--           \--------/     

exec :: SM a b -> [a] -> (SM a b, [b])
```
From the theoretical perspective, this model is a simplified version of FRP, but adding states on functions directly. In another word, it is switching the focus from time to states.

From the engineering perspective, the other difference from AFRP(Yampa) is that we provide the constructor to use the transition function ```trans :: s -> a -> (SM a b, b)``` to build ```SM a b``` directly. 

### Simplifed model

In functional reactive programming(FRP), the key concepts are the signal, ```Signal a :: Time -> a```, and the signal function from signal to signal, ```SF a b :: Signal a -> Signal b```.

The model of FRP is beautiful, but one diffcult thing is that the signal is continuous function, and our computers are discrete systems. 

However, what if we do not care about time, and only focus on the sequence of input. There is reason to believe that computational tasks usually are time-insensitive. For example, the parsing process. So ```[a]``` and ```[Event a]``` are the only things we expected in our system.

For discrete system, simplifying the input type is kind of generalizing ```[(Time,a)]``` to ```[a]```. This simplifed model is still able to process the time sequences by using ```[(Time, a)]``` as the input. In conclusion, we doesn't consider time as an essential part of the input, but if the input involves time, users can add time back as a part of the input.

### Stateful function(Storage)

Usually, the state can be abstracted to the summary of input history. With the ArrowLoop instance, we can create stateful functions in FPR. For example, if we want to get a function ```SF a b``` with state ```c```. We first write a function ```SF (a, c) (b, c)```, then use ```loop :: SF (a, c) (b, c) -> SF a b``` to get the stateful function ```SF a b```.

But I prefer to think the pure function as the stateful function with state ```()```, because the stateful function gives us a more natural way to represent the control flow. Also, it give us the ability to switch the function itself based on different inputs.

## Implementation

The key idea is using the GADTs extension to hide the state(storage) type. If we do not use the GADTs extension, then ```SM a b``` will become ```SM s a b``` where ```s``` denotes the state(storage) type.

## To-Do
  * Basic state machines
  * Event
  * More high order functions
  * Another DSL to build transition functions?
  
## References

[Functional Reactive Programming, Continued](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/workshop-02.pdf)

[Yampa - Haskell Wiki](https://wiki.haskell.org/Yampa)
