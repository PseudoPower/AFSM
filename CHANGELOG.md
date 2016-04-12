## 0.1.2.0

  * switch ```SM :: s -> (SMState s a b) -> SM a b``` to ```SM :: (SMState s a b) -> s -> SM a b```.
  * the ArrowApp instance
  * new functions: foldlSM, foldlDelaySM, delaySM, concatSM.
  * working on Event, several undefined functions about Event.
  * It's always hard to pick a name! 'Event', 'Evnt', 'Ev' or 'E'? 