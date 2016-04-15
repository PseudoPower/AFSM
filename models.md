# Models related to our project

##Models

Definitions:
  * AFRP: `Signal a :: Time -> a`, and `SF a b :: Signal a -> Signal b`.
  * State: `type State s a b = (s, a) -> (s, b)`.
  * ST Monad: `data ST s a = State { runState :: s -> (a, s) }`.
  * Circuit: `newtype Circuit a b = Circuit (a -> (Circuit a b, b))`.
  * AFSM: `TF s a b = TF (s -> a -> (SM s a b, b))` and `SM s a b = SM (TF s a b) s`.

Because all of them are the abstraction about computation. So they have the same abilities, Everything you can do in one model, you also can do it in other models. Even we can believe `type Func = ([a]->[b])` can do all the same thing, because all of them are in the pure world.

### AFRP

AFRP doesn't open the constructor to users, and this limilt it abilities. But it is able to switch the signal functions base on the input by using switchers.

### State

State model is keeping a global state(storage), and sharing changings between functions. To have the local storage for functions, we cannot use the same techique which used in Circuit model, because the transition function is fixed. But we can use ArrowLoop to have the local variables.

the meaning of storage(state) are very different between this model and AFSM. In this model, the storage is shared between functions, so a function modify the storage and sends the new storage to the next function. As it said in this chapter, it is a threading of a state through a function. So the storage in this model represents the gobal variables. But in our model, each machine maintains its own storage, so it represents the local variables. If our users want to introduce the global variables, the only thing they need to do is just modifying the input type from `a` to `(g, a)`. And modifying the input type from `a` to `(g, a)` is what this model did in this chapter.


### ST Monad

ST Monad is a simpler version of State. ST is the same with `State s () b`. But we also can define `ST s ([a]->[b])` make them as the same.

### Circuit

### Ways to have the local storage

1. If we can switch the transition function dynamically, TF a b = (a -> (TF a b, b)). It gives us a chance to hide the local variables into the transition function.

2. Also, if it is an instance of ArrowLoop, using `delay` and `loop` can have local variables. This also requires the dynamically switching feature, and it is more complex than the first one, but the benefit is that it is able to keep the information of the storage.

3. Otherwise, for example, `ST s ([Int]->[Int])`, we should Build `TF a b = (a -> (TF a b, b))` by ourselves in the output function `[Int] -> [Int]`.

## Compare to other models 

This table just shows that whether these features are easy to have in these models.

|            | AFRP        | Circuit    | ST Monad | State   | AFSM       |
|:----------:|:-----------:|:----------:|:--------:|:-------:|:----------:|
| Arrow      | Yes         | Yes        | No       | Yes     | Partial    |
| Input      | [(DTime,a)] | [a]        | No       | [a]     | [a]        |
| Output     | [(DTime,b)] | [b]        | b        | [b]     | [b]        |
| Storage    | No          | Flexible s | Fixed s  | Fixed s | Flexible s |
| Scope      | No          | Local      | Global   | Global  | Local      |
| Extraction | No          | No         | Output   | Output  | Yes        | 
| Transition | Yes         | Yes        | No       | No      | Yes        |

These models are included in the references.

Arrow represents whether it is an instance of the Arrow type class.

Input represents the type of inputs.

Output represents the type of outputs, `ST Monad` has no input sequence, so it just returns a value.

Storage represents whether it has build-in storage. These models which have the global storage usually require a initial value of storage. It seems that `Circuit a b = Circuit (a -> (Circuit a b, b))` doesn't have the storage. But that is easy to make it has the storage, and it is exactly what we are doing. `newStateCircuit f s = Circuit (f s)`, then the `newStateCircuit :: (s -> a -> (Circuit a b)) -> s -> Circuit a b` has the same function with what our constructor does. This fact may focuses us to remove GADTs extension, and keep track of the storage type.

Scope represents whether the storage is global or local. In `ST Monad` and `State` model, all functions share the storage with each others, that means that a function change the storage, and send the updated storage to the next function. That's why the type of their storage is fixed. But in our model, each state machine has its own storage and the types of storage in different machines can be different. And machines cannot access other machines' storage, so the storage in our model represents the local variables! And if we want a global storage, and share it between all the machines, we just need to change the input type from `[a]` to `[(g, a)]` where `g` denotes the type of the global storage.

Extraction means whether it is easy to extract the local storage, and after extraction, could we load the storage back?

Transitinon represents whether it can switch the transition function dynamically.

Base on their own models, both `AFRP` and `AFSM` have the Event type.

## References
[Functional Reactive Programming, Continued](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/workshop-02.pdf)
  * the paper about AFRP. Actually, AFRP is simulating signal systems, also it's why I prefer to use the name `signal function` instead of `behavior function`. On the other hand, it looks like that the transition function(TF) of AFRP is the same with the AFSM's TF if we fix storage type with `DTime`, and the benefit is that it does not require the GADTs extension. However, the real diffence between them is that AFRP doesn't store the DTime, because DTime just comes from the input. So the biggest diffence between our model and others, is that we have a more netural way to repesent the local variables!

[Yampa - Haskell Wiki](https://wiki.haskell.org/Yampa)
  * the implementation of AFRP

[Control.Monad.ST](https://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Monad-ST.html)

[Arrows and computation](http://ipaper.googlecode.com/git-history/243b02cb56424d9e3931361122c5aa1c4bdcbbbd/Arrow/arrows-fop.pdf)
  * the model in this chapter is named state transformers, I just call it `State` model.  The `Auto` model in this chapter is the same with `Circuit`.
  * Our professor points out that this chapter is very similiar with what we are doing. After reading, we realize that the `State` medol and our model are very different. But!!! we find that our model is almost the same with `Circuit` model!!!
  * About the `State` model: 
[Haskell/Arrow tutorial](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial)
  * the Arrow tutorial with `Circuit` model. Here is its definition, `newtype Circuit a b = Circuit (a -> (Circuit a b, b))`. 
  * It seems that `Circuit` doesn't have the storage. But that is easy to make it has the storage, and it is exactly what we are doing. `newStateCircuit f s = Circuit (f s)`, then the `newStateCircuit :: (s -> a -> (Circuit a b)) -> s -> Circuit a b` has the same function with what our constructor does.
  * So it the same with our model, this made me feel a little bit sad. But there should be someone in the world have already found this delicate model, right?
  * Another thing is that we will introduce the `Event` type into our system, which the `Circuit` doesn't have. And evnet makes building `slowdownSM :: SM [a] (Event a)` become a possible thing.
  * In the end, I am still a litte frustrated. There is one thing should be clear(probably it is a little pessimistic). `Circuit`, `AFRP`, `AFSM` and the model in this chapter are all Turing complete and having instances of the Arrow type class. Once they have the instance of ArrowLoop or changing the transition function dynmically, it has the ability to be stateful. And once they have the instance of ArrowChoice, they have the ability to perform different computations for different inputs. It is kind of saying "the assembly language can do anything, and why we need a new language". Without considering the ability, whether the design is convenient (and efficient) for users is the most important things for me to keep in mind. These words sound like an old man's words! :D