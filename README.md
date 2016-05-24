# Arrowized functional state machines

Because this readme file becomes so long, I moved other models to [models.md](docs/models.md), and I am able to detail each of them without worrying about the length of the article, :)

## Update notes

Frankly, I still love the idea about hiding the storage, so I add one more data type `data SF a b = SF (a -> (SF a b, b))` which represents stateful functions. It is the same with `SMH`, `Circuit` and `Auto`, but I love it, and just put it beside the state machines, :)

I find a more abstractive thing, [Automaton](https://hackage.haskell.org/package/arrows-0.4.4.1/docs/Control-Arrow-Transformer-Automaton.html). But I want to make it less abstactive, and easier to use. So I add a interesting data type `data SFM m a b = forall m. (Monad m) => SFM (a -> m (SFM m a b, b))`. Now, the pure and impure worlds are merged.

There is another thing `IOSF` which is inspired by the keyword `asym` in Elm. But I don't know much about the concurrent FRP, so I haven't figure out which one is the right abstaction, and it may be rewritten in future. Briefly, now it is `type ThreadSF a b = (TChan a) -> IO (TChan b)`, and the constructor is `newThreadSF :: Foldable t => SF a (t b) -> IO (ThreadSF a b)`.

## Introduction

The intuitive way to understand the state machine is that it have four parts, state, storage, input and output. Each state has its own transition function which takes the storage and the input, updates the storage and the state, and gives the output.

The abstract way is thinking the state machine as the stateful function.

Now, let us put these two ways together. Our plan is using the stateful function as the interface, but users can build state machines in an intuitive way.

## Basic Concepts

The `SM a b` type denotes stateful functions from `a` to `b`. Also, It is an instance of the `Arrow` type class.
```
-- | 'TF' is a type representing a transition function.
--     s: storage, a: input, b: output
--   Also, it represents the state in SM.
--   Let's explain more about 'TF'. When a state gets an input a,
--   it should do three things base on the storage and input:
--     find the next state(TF), update storage and output b.
--   That's why it looks like this:
--     (storage -> a -> (SM newTF newStorage, b))
type TF s a b = (s -> a -> (SM a b, b))

-- | 'SM' is the type representing state machines.
data SM a b where
  SM :: (TF s a b) -> s -> SM s a b
--
--    a  /----------\  b
--  >--->| SM s a b |>--->
--       \----------/
--
-- Here is a slight difference from Arrow class,
--   so `SM s a b` is not an instance of Arrow class,
--   but you can do everything the same as an Arrow instance.
-- And `SM () a b` is still an instance of Arrow,
--   and `hideStorage :: SM s a b -> SM () a b` can help you to achieve easily.
-- It is a trade-off between keeping track the storage and having the instance of Arrow.
--
-- (>>>>) :: SM s a b -> SM t b c -> SM (s,t) a c
--
--    a  /----------\  b  /----------\  c
--  >--->| SM s a b |>--->| SM t b c |>--->
--       \----------/     \----------/
--
--
-- firstSM :: SM s a b -> SM s (a, c) (b, c)
--
--    a  /----------------\  b
--  >--->|>-> SM s a b >->|>--->
--       |                |
--  >--->|>-------------->|>--->
--    c  \----------------/  c
--
--
-- (****) :: SM s a b -> SM t c d -> SM (s,t) (a, c) (b, d)
--
--    a  /----------------\  b
--  >--->|>-> SM s a b >->|>--->
--       |                |
--  >--->|>-> SM t c d >->|>--->
--    c  \----------------/  d
--
--
-- (&&&&) :: SM s a b -> SM t a c -> SM (s,t) a (b, c)
--
--            /----------\  b
--       /--->| SM s a b |>---\
--    a  |    \----------/    |  (b,c)
--  >--->|                    |>------->
--       |    /----------\  c |
--       \--->| SM t a c |>---/
--            \----------/
--

-- execute SM s a b with inputs,
--   you can think `SM s a b` is a stateful function `f :: a -> b` with storage `s`.
--   `step` is the apply function. `exec` is the mapping function.
-- We also introduce our own Functor class, `SMFunctor`.
--   And `smexec :: SMFunctor f => SM s a b -> f a -> (SM s a b, f b)`.

step :: SM s a b -> a -> (SM s a b, b)

exec :: SM s a b -> [a] -> (SM s a b, [b])

smfmap :: SM s a b -> [a] -> [b]
```

From a theoretical point of view, this model is a simplified version of FRP, but adding states on functions directly. In another word, it is switching the focus from time to states.

From an engnieering point of view, the other difference from AFRP(Yampa) is that we provide the constructor to use the transition function `TF s a b :: s -> a -> (SM s a b, b)` to build `SM s a b` directly where `s` donates the storage type.

### Simplified model

In functional reactive programming(FRP), the key concepts are the signal, `Signal a :: Time -> a`, and the signal function from signal to signal, `SF a b :: Signal a -> Signal b`.

The model of FRP is beautiful, but one difficult thing is that the signal is continuous function, and our computers are discrete systems.

However, what if we do not care about time, and only focus on the sequence of input. There is reason to believe that computational tasks usually are time-insensitive. For example, the parsing process. So `[a]` and `[Event a]` are the only things we expected in our system.

For discrete system, simplifying the input type is kind of generalizing `[(Time,a)]` to `[a]`. This simplified model is still able to process the time sequences by using `[(Time, a)]` as the input. In conclusion, we doesn't consider time as an essential part of the input, but if the input involves time, users can add time back as a part of the input.

### Stateful function(Storage)

Usually, the state can be abstracted to the summary of input history. With the ArrowLoop instance, we can create stateful functions in FPR. For example, if we want to get a function `SF a b` with state `c`. We first write a function `SF (a, c) (b, c)`, then use `loop :: SF (a, c) (b, c) -> SF a b` to get the stateful function `SF a b`.

But I prefer to think the pure function as the stateful function with state `()`, because the stateful function gives us a more natural way to represent the control flow. Also, it give us the ability to switch the function itself based on different inputs.

## Implementation

There are two similar implementation now. One is keeping the storage type and is not an instance of arrow. The other one is hiding the storage type and is an instance of arrow.

### State machine

`data SM s a b = SM (TF s a b) s`
  * `newSM :: (s -> a -> (SM s a b, b)) -> s -> SM s a b`, and `simpleSM :: (s -> a -> (s, b)) -> s -> SM s a b`
  * `instance Arrow (SM ())`, and `hideStorage :: SM s a b -> SM () a b`
  * `class SMFunctor f`, and `smexec :: SM s a b -> f a -> (SM s a b, f b)`

`data TF s a b = TF (s -> a -> (SM s a b, b))`
  * `transSM2TF :: SM t (s, a) (s, b) -> TF s a b`
  * `instance Arrow TF`

### Stateful function

`data SF a b = SF (a -> (SF a b, b))`
  * `newSF :: (s -> a -> (SF a b, b)) -> s -> SF a b`, and `simpleSF :: (s -> a -> (s, b)) -> s -> SF a b`
  * `instance Arrow SF`
  * `class SFunctor f` and `sfexec :: SF a b -> f a -> (SF a b, f b)`

`data STF s a b = STF (s -> a -> ((STF s a b, s), b))`
  * `transSTF2SF :: STF s a b -> s -> SF a b`
  * `instance Arrow STF`
  
`newtype SFM m a b = SFM (a -> m (SFM m a b, b))`
  * `newSFM :: (Monad m) => (s -> a -> m (SFM m a b, b)) -> s -> m (SFM m a b)`
  * `simpleSFM :: (Monad m) => (s -> a -> m (s, b)) -> s -> m (SFM m a b)`
  * `instance Monad m => Arrow (SFM m)`
  
`type ThreadSF a b = (TChan a) -> IO ([ThreadId], TChan b)`
  * `sf2TSF :: Foldable t => SF a (t b) -> IO (ThreadSF a b)`
  * `sfm2TSF :: Foldable t => SFM IO a (t b) -> IO (ThreadSF a b)`
  * `idTSF :: ThreadSF a a` and `composeTSF :: ThreadSF b c -> ThreadSF a b -> ThreadSF a c`

## Examples

### HelloWorld([HelloWorld.hs](https://github.com/PseudoPower/AFSM/blob/master/examples/SM/HelloWorld.hs))

It shows basic components of AFSM. And an example about operating stacks.

### Toys([Toys.hs](https://github.com/PseudoPower/AFSM/blob/master/examples/SM/Toys.hs))

**A collection of toys**

The Fibonacci sequence

The random number sequence

The prime number sequence

### Reverse Polish notation([RPN.hs](https://github.com/PseudoPower/AFSM/blob/master/examples/SM/RPN.hs))

To run this example, just type `make RPN` or `ghci examples/RPN.hs -isrc/`. The makefile will be maintained for all examples. Then you can run `main` function and type some infix expressions, such as  `3 * (2 - 3) + (4 - 2 * 3), 3 + 4 * 2 / (1 - 5) * 2 + 3`.

It is also known as postfix notation, and it is very straightforward example. The input is the infix expression, and the output is the value. First, we build a SM named in2post to convert infix notation to postfix expression. Then we build a SM named post2ret to evaluate the values. Finally, we use them to compose `in2ret = in2post >>> post2ret`.

### Clock([Clock.hs](https://github.com/PseudoPower/AFSM/blob/master/examples/SF/Clock.hs))

A Clock implementation using `SDL2` library. It shows how to use `ThreadSF`.

## To-Do
  * Basic state machines (continuous adding some new SMs.)
  * Event (It is similar with the Maybe data type, but in our control, and after defining Event, we can do some interesting things.)
  * TF (compose some TFs to one TF, it means several SM shares the same storage.)
  * More high order functions (distinguish which functions are essential.)
  * Another DSL to build transition functions? (It is not necessary now.)

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
