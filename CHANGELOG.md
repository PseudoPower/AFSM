## 0.1.3.0
  * remove GADTs extension, `SM a b` becomes `SM s a b`.
  * change `type SMState s a b = (s -> a -> (SM s a b, b))` to `newtype TF s a b = TF (s -> a -> (SM s a b, b))`. The reason is that TF is a SM without initial storage, and it is the instance of several class type.
  * change `data SM s a b = SM (SMState s a b) s` to `data SM s a b = SM (TF s a b) s`.
  * `SM s a b` is no longer an instance of Arrow, but `SM () a b` is still an instance of Arrow. `type SMH a b = SM () a b`.
  * the `SMFunctor` class, and `smfmap` helps you to use `SM s a b` as a normal function. 

## 0.1.2.0

  * switch `SM :: s -> (SMState s a b) -> SM a b` to `SM :: (SMState s a b) -> s -> SM a b`.
  * the ArrowApp instance
  * new functions: foldlSM, foldlDelaySM, delaySM, concatSM.
  * working on Event, several undefined functions about Event.
  * It's always hard to pick a name! 'Event', 'Evnt', 'Ev' or 'E'? 
 