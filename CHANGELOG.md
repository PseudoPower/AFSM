## 0.1.3.1
  * add SF data type. It is the type of stateful functions, and it is the same with SMH type without empty storage.

## 0.1.3.0
  * remove GADTs extension, `SM a b` becomes `SM s a b`.
  * change `type SMState s a b = (s -> a -> (SM s a b, b))` to `newtype TF s a b = TF (s -> a -> (SM s a b, b))`. The reason is that TF is a SM without initial storage, and it is the instance of several class type.
  * change `data SM s a b = SM (SMState s a b) s` to `data SM s a b = SM (TF s a b) s`.
  * `SM s a b` is no longer an instance of Arrow, but `SM () a b` is still an instance of Arrow. `type SMH a b = SM () a b`.
  * the `SMFunctor` class, and `smfmap` helps you to use `SM s a b` as a normal function. 
  * more examples.
  
Now, the GADTs extension has been removed, and a lot of things should been cleared up. For now, I just keep them right there.

`SM a b` has became to `SM s a b`, so it is not an instance of the Arrow class anymore. Instead, we are able to keep the information about each machine's storage. Also, we provide the same Arrow functions, such as `<<<<`, `>>>>`, `****` and `&&&&`.

`type SMH a b = SM () a b` is still an Arrow instance, and `hideStorage :: SM s a b -> SM () a b` can help you transform `SM s a b` to `SM () a b`, if you want to use the Arrow notation. The cost is that the information about storage is gone.

`type SMState s a b = (s -> a -> (SM a b, b))` becomes `data TF = TF (s -> a -> (SM a b, b))`. The reason about this changing is that `type SMState` is just defining an alias of an existing type, So it is not possible to define a instance for `SMState`. We observed that  `(s -> a -> (SM a b, b))` is similiar with `ST monad`, and its behavior is also similiar with `ST monad`. I think it is a good chance to do something around.

Although our original idea is removing `Time` concept and adding `Storage` concept, now it is much closer with `Circuit` model or `State` model than `AFRP`. More precisely, it is a mixture of `Circuit` and `State`.


## 0.1.2.0

  * switch `SM :: s -> (SMState s a b) -> SM a b` to `SM :: (SMState s a b) -> s -> SM a b`.
  * the ArrowApp instance
  * new functions: foldlSM, foldlDelaySM, delaySM, concatSM.
  * working on Event, several undefined functions about Event.
  * It's always hard to pick a name! 'Event', 'Evnt', 'Ev' or 'E'? 
 