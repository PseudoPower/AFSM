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

## Compare to other models

|            | AFRP        | Circuit    | ST Monad | State   | AFSM       |
|:----------:|:-----------:|:----------:|:--------:|:-------:|:----------:|
| Arrow      | Yes         | Yes        | No       | Yes     | Yes        |
| Input      | [(DTime,a)] | [a]        | No       | [a]     | [a]        |
| Output     | [(DTime,b)] | [b]        | b        | [b]     | [b]        |
| Storage    | No          | Flexible s | Fixed s  | Fixed s | Flexible s |
| Scope      | No          | Local      | Global   | Global  | Local      |
| Transition | Yes         | Yes        | No       | No      | Yes        |

These models are included in the references.

Arrow represents whether it is an instance of the Arrow type class.

Input represents the type of inputs.

Output represents the type of outputs, ```ST Monad``` has no input sequence, so it just returns a value.

Storage represents whether it has build-in storage. These models which have the global storage usually require a initial value of storage. It seems that ```Circuit a b = Circuit (a -> (Circuit a b, b))``` doesn't have the storage. But that is easy to make it has the storage, and it is exactly what we are doing. ```newStateCircuit f s = Circuit (f s)```, then the ```newStateCircuit :: (s -> a -> (Circuit a b)) -> s -> Circuit``` has the same function with what our constructor does. This fact may focuses us to remove GADTs extension, and keep track of the storage type.

Scope represents whether the storage is global or local. In ```ST Monad``` and ```State``` model, all functions share the storage with each others, that means that a function change the storage, and send the updated storage to the next function. That's why the type of their storage is fixed. But in our model, each state machine has its own storage and the types of storage in different machines can be different. And machines cannot access other machines' storage, so the storage in our model represents the local variables! And if we want a global storage, and share it between all the machines, we just need to change the input type from ```[a]``` to ```[(g, a)]``` where ```g``` denotes the type of the global storage.

Transitinon represents whether it can switch the transition function dynamically.

Base on their own models, both ```AFRP``` and ```AFSM``` have the Event type.

## References
[Functional Reactive Programming, Continued](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/workshop-02.pdf)
  * the paper about AFRP. Actually, AFRP is simulating signal systems, also it's why I prefer to use the name ```signal function``` instead of ```behavior function```. On the other hand, it looks like that the transition function(TF) of AFRP is the same with the AFSM's TF if we fix storage type with ```DTime```, and the benefit is that it does not require the GADTs extension. However, the real diffence between them is that AFRP doesn't store the DTime, because DTime just comes from the input. So the biggest diffence between our model and others, is that we have a more netural way to repesent the local variables!

[Yampa - Haskell Wiki](https://wiki.haskell.org/Yampa)
  * the implementation of AFRP

[Control.Monad.ST](https://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Monad-ST.html)

[Arrows and computation](http://ipaper.googlecode.com/git-history/243b02cb56424d9e3931361122c5aa1c4bdcbbbd/Arrow/arrows-fop.pdf)
  * the model in this chapter is named state transformers, I just call it ```State``` model.  The ```Auto``` model in this chapter is the same with ```Circuit```.
  * Our professor points out that this chapter is very similiar with what we are doing. After reading, we realize that the ```State``` medol and our model are very different. But!!! we find that our model is the same with ```Circuit``` model!!!
  * About the ```State``` model: the meaning of storage(state) are very different between this model and AFSM. In this model, the storage is shared between functions, so a function modify the storage and sends the new storage to the next function. As it said in this chapter, it is a threading of a state through a function. So the storage in this model represents the gobal variables. But in our model, each machine maintains its own storage, so it represents the local variables. If our users want to introduce the global variables, the only thing they need to do is just modifying the input type from ```a``` to ```(g, a)```. And modifying the input type from ```a``` to ```(g, a)``` is what this model did in this chapter.

[Haskell/Arrow tutorial](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial)
  * the Arrow tutorial with ```Circuit``` model. Here is its definition, ```newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }```. 
  * It seems that ```Circuit``` doesn't have the storage. But that is easy to make it has the storage, and it is exactly what we are doing. ```newStateCircuit f s = Circuit (f s)```, then the ```newStateCircuit :: (s -> a -> (Circuit a b)) -> s -> Circuit``` has the same function with what our constructor does.
  * So it the same with our model, this made me feel a little bit sad. But there should be someone in the world have already found this delicate model, right?
  * Another thing is that we will introduce the ```Event``` type into our system, which the ```Circuit``` doesn't have. And evnet makes building ```slowdownSM :: SM [a] (Event a)``` become a possible thing.
  * In the end, I am still a litte frustrated. There is one thing should be clear(probably it is a little pessimistic). ```Circuit```, ```AFRP```, ```AFSM``` and the model in this chapter are all Turing complete and having instances of the Arrow type class. Once they have the instance of ArrowLoop, it has the ability to be stateful. And once they have the instance of ArrowChoice, they have the ability to perform different computations for different inputs. It is kind of saying "the assembly language can do anything, and why we need a new language". Without considering the ability, whether the design is convenient (and efficient) for users is the most important things for me to keep in mind. These words sound like an old man's words! :D
