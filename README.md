# Arrowized functional state machines

Because this readme file becomes so long, I moved other models to [models.md](https://github.com/PseudoPower/AFSM/blob/master/models.md), and I am able to detail each of them without worrying about the length of the article, :)

## Latest changing

Now, the GADTs extension has been removed, and a lot of things should been cleared up. For now, I just keep them right there.

`SM a b` has became to `SM s a b`, so it is not an instance of the Arrow class anymore. Instead, we are able to keep the information about each machine's storage.

`type SMH a b = SM () a b` is still an Arrow instance, and `hideStorage :: SM s a b -> SM () a b` can help you transform `SM s a b` to `SM () a b`, if you want to use the feature of Arrow class. The cost is that the information about storage is gone.

`type SMState s a b = (s -> a -> (SM a b, b))` becomes `data TF = TF (s -> a -> (SM a b, b))`. The reason about this changing is that `type SMState` is just defining an alias of an existing type. So it is not possible to define a instance for `SMState`. We observed that  `(s -> a -> (SM a b, b))` is similiar with `ST monad`, and its behavior is also similiar with `ST monad`. I think it is a good chance to do something around.

Although our original idea is removing `Time` concept and adding `Storage` concept, now it is much closer with `Circuit` model or `State` model than `AFRP`.

## Introduction

The intuitive way to understand the state machine is that it have four parts, state, storage, input and output. Each state has its own transition function which takes the storage and the input, updates the storage and the state, and gives the output.

The abstract way is thinking the state machine as the stateful function.

Now, let us put these two ways together. Our plan is using the stateful function as the interface, but users can build state machines in an intuitive way.

## Basic Concepts

The `SM a b` type denotes stateful functions from `a` to `b`. Also, It is an instance of the `Arrow` type class.
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

From an engnieering point of view, the other difference from AFRP(Yampa) is that we provide the constructor to use the transition function `SMState s a b :: s -> a -> (SM a b, b)` to build `SM a b` directly where `s` donates the storage type.

### Simplifed model

In functional reactive programming(FRP), the key concepts are the signal, `Signal a :: Time -> a`, and the signal function from signal to signal, `SF a b :: Signal a -> Signal b`.

The model of FRP is beautiful, but one diffcult thing is that the signal is continuous function, and our computers are discrete systems.

However, what if we do not care about time, and only focus on the sequence of input. There is reason to believe that computational tasks usually are time-insensitive. For example, the parsing process. So `[a]` and `[Event a]` are the only things we expected in our system.

For discrete system, simplifying the input type is kind of generalizing `[(Time,a)]` to `[a]`. This simplifed model is still able to process the time sequences by using `[(Time, a)]` as the input. In conclusion, we doesn't consider time as an essential part of the input, but if the input involves time, users can add time back as a part of the input.

### Stateful function(Storage)

Usually, the state can be abstracted to the summary of input history. With the ArrowLoop instance, we can create stateful functions in FPR. For example, if we want to get a function `SF a b` with state `c`. We first write a function `SF (a, c) (b, c)`, then use `loop :: SF (a, c) (b, c) -> SF a b` to get the stateful function `SF a b`.

But I prefer to think the pure function as the stateful function with state `()`, because the stateful function gives us a more natural way to represent the control flow. Also, it give us the ability to switch the function itself based on different inputs.

## Implementation

The key idea is using the GADTs extension to hide the state(storage) type. If we do not use the GADTs extension, then `SM a b` will become `SM s a b` where `s` denotes the state(storage) type. However, after hiding the storage type, it is the same with `Circuit a b`. The funny thing is that we come from AFRP, and end up with Circuit.

We are planning to removing GADTs extension, then `SM a b` becomes `SM s a b`. The benifit is that we can extract the storage from a SM, and the limitation is that we cannot put itself as the storage or do something may cause inifinite type. Also, when we put several SMs together, the type of storage will be in a mess. It's the reason we use GADTs to hide the type of storage before, but now we still have a way to hide the storage if you never want to extract it, `hideStorage :: SM s a b -> SM () a b`. 

## Examples

### Reverse Polish notation([RPN.hs](https://github.com/PseudoPower/AFSM/blob/master/examples/RPN.hs))

To run this example, just type `make RPN` or `ghci examples/RPN.hs -isrc/`. The makefile will be maintained for all examples. Then you can run `main` function and type some infix expressions, such as  `3 * (2 - 3) + (4 - 2 * 3), 3 + 4 * 2 / (1 - 5) * 2 + 3`.

It is also known as postfix notation, and it is very straightforward example. The input is the infix expression, and the output is the value. First, we build a SM named in2post to convert infix notation to postfix expression. Then we build a SM named post2ret to evaluate the valus. Finally, we use them to compose `in2ret = in2post >>> post2ret`.


## To-Do
  * Basic state machines
  * Event
  * More high order functions
  * Another DSL to build transition functions?

## References
[Functional Reactive Programming, Continued](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/workshop-02.pdf)
  * the paper about AFRP. Actually, AFRP is simulating signal systems, also it's why I prefer to use the name `signal function` instead of `behavior function`.
  * `Signal a :: Time -> a`, and `SF a b :: Signal a -> Signal b`.

[Yampa - Haskell Wiki](https://wiki.haskell.org/Yampa)
  * the implementation of AFRP
  * `type Transition a b = (SF a b, b)`, and `data SF a b = SF (DTime -> a -> Transition a b)`.

[Control.Monad.ST](https://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Monad-ST.html)
  * State Monad
  * `data ST s a = State { runState :: s -> (a, s) }`.

[Arrows and computation](http://ipaper.googlecode.com/git-history/243b02cb56424d9e3931361122c5aa1c4bdcbbbd/Arrow/arrows-fop.pdf)
  * the model in this chapter is named state transformers, I just call it `State` model.  The `Auto` model in this chapter is the same with `Circuit`.
  * `type State s a b = (s, a) -> (s, b)`.

[Haskell/Arrow tutorial](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial)
  * the Arrow tutorial with `Circuit` model. 
  * `newtype Circuit a b = Circuit (a -> (Circuit a b, b))`. 
