**0.1.2.0**
  * switch ```SM :: s -> (SMState s a b) -> SM a b``` to ```SM :: (SMState s a b) -> s -> SM a b```.