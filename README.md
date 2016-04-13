# Arrowized functional state machines

## Introduction

The intuitive way to understand the state machine is that it have four parts, state, storage, input and output. Each state has its own transition function which takes the storage and the input, updates the storage and the state, and gives the output.

The abstract way is thinking the state machine as the stateful function.

Now, let us put these two ways together. Our plan is using the stateful function as the interface(making it to be an instance of the ```Arrow``` type class), but users can build state machines in an intuitive way.

## Basic Concepts

The ```SM a b``` type denotes stateful functions from ```a``` to ```b```. Also, It is an instance of the ```Arrow``` type class.
```
-- | 'SMState' is a type representing a transition function.
--     s: storage, a: input, b: output
--   Let's explain more about 'SMState'. When a state gets an input a, 
--   it should do three things base on the storage and input: 
--     find the next state, update storage and output b.
--   That's why it looks like this:
--     (storage -> a -> (SM newState newStorage, b))
type SMState s a b = (s -> a -> (SM a b, b))

-- | 'SM' is the type representing state machines.
data SM a b where
  SM :: (SMState s a b) -> s -> SM a b
--
--    a  /--------\  b
--  >--->| SM a b |>--->
--       \--------/
--
-- (>>>) :: SM a b -> SM b c -> SM a c
--
--    a  /--------\  b  /--------\  c
--  >--->| SM a b |>--->| SM b c |>--->
--       \--------/     \--------/
--
--
-- first :: SM a b -> SM (a, c) (b, c)
--
--    a  /--------------\  b
--  >--->|>-> SM a b >->|>--->
--       |              |
--  >--->|>------------>|>--->
--    c  \--------------/  c
--
-- (***) :: SM a b -> SM c d -> SM (a, c) (b, d)
--
--    a  /--------------\  b
--  >--->|>-> SM a b >->|>--->
--       |              |
--  >--->|>-> SM c d >->|>--->
--    c  \--------------/  d
--
-- (&&&) :: SM a b -> SM a c -> SM a (b, c)
--
--            /--------\  b
--       /--->| SM a b |>---\
--    a  |    \--------/    |  (b,c)
--  >--->|                  |>------->
--       |    /--------\  c |
--       \--->| SM a c |>---/
--            \--------/
--

-- execute SM a b with input [a].
exec :: SM a b -> [a] -> (SM a b, [b])
```

From a theoretical point of view, this model is a simplified version of FRP, but adding states on functions directly. In another word, it is switching the focus from time to states.

From an engnieering point of view, the other difference from AFRP(Yampa) is that we provide the constructor to use the transition function ```SMState s a b :: s -> a -> (SM a b, b)``` to build ```SM a b``` directly where ```s``` donates the storage type.

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

## Examples

### Reverse Polish notation([RPN.hs](https://github.com/PseudoPower/AFSM/blob/master/examples/RPN.hs))

To run this example, just type ```make RPN``` or ```ghci examples/RPN.hs -isrc/```. The makefile will be maintained for all examples. Then you can run ```main``` function and type some infix expressions, such as  ```3 * (2 - 3) + (4 - 2 * 3), 3 + 4 * 2 / (1 - 5) * 2 + 3```.

It is also known as postfix notation, and it is very straightforward example. The input is the infix expression, and the output is the value. First, we build a SM named in2post to convert infix notation to postfix expression. Then we build a SM named post2ret to evaluate the valus. Finally, we use them to compose ```in2ret = in2post >>> post2ret```.


## To-Do
  * Basic state machines
  * Event
  * More high order functions
  * Another DSL to build transition functions?

## References

[Functional Reactive Programming, Continued](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/workshop-02.pdf)

[Yampa - Haskell Wiki](https://wiki.haskell.org/Yampa)

[Haskell/Arrow tutorial](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial)

  * Just realize that both AFRP and our model are very similar with ```Circuit```. Here is its definition, ```newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }```. Actually, FRP is simulating signal systems, also it's why I prefer to use the name ```signal function``` instead of ```behavior function```. On the other hand, AFRP is AFSM with fix storage type ```DTime```, and the benefit is that it does not require the GADTs extension.

[Arrows and computation](http://ipaper.googlecode.com/git-history/243b02cb56424d9e3931361122c5aa1c4bdcbbbd/Arrow/arrows-fop.pdf)

  * Our professor point out that this chapter is very similiar with what we are doing. I haven't finished reading, but there is bad news and good news based on the first impression. 
  * The bad news is that it is very very similiar with our model, this made me feel a little bit sad. But there should be someone in the world have already found this delicate model, right? But there also has some good news, :) This chapter models the "applications that involve the threading of a state through a function". Let's take a look at his state type, ```type State s a b :: (s, a) -> (s, b)```. It updates the storage and gives output base on the storage and input. Frankly, it has the same basic idea with us.
  * There are two good news. First, it only update the storage, so it cannot switch the transition function dynamically. Second, because this chapter is too old, maybe they didn't have the GADTs extension at that moment(2003?). So they didn't hide the storage type, and it means that it requires all the functions has the same stroage type, ```instance Arrow (State s)```. The different state machines may have different storage types, and this sounds like a more intuitive model to me.
  * Another thing is that we will introduce the Event type into our system, which this chapter doesn't have. And it makes building ```slowdownSM :: SM [a] (Event a)``` become a possible thing.
  * In the end, I am still a litte frustrated. There is one thing should be clear(probably it is a little pessimistic). ```Circuit```, ```AFRP```, ```AFSM``` and the model in this chapter are all Turing complete. And once they have the instance of ArrowLoop, it has the ability to be stateful. And once they have the instance of ArrowChoice, they have the ability to perform different computations for different inputs. It is kind of saying "the assembly language can do anything, and why we need a new language". Without considering the ability, whether the design is convenient for users is the most important things for me to keep in mind. These words sound like an old man's words! :D
