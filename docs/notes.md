# Development notes

## Ideas

### PartialTypeSignatures

```
{-# LANGUAGE PartialTypeSignatures #-}

f :: Int -> _
f x = x + 1
```

### Pretty Print

I tried to print the SM as the circuit diagram. Drawing wires to connect two SMs(Once we know where are inputs and outputs) is not very hard, but the hard part is to infer the input size and the output size. Actually, it is a HM inference type system.

This part is easy:
```
>-----+
>---+ |
>-+ | +->
  | |   
  | +--->
  +----->
  +----->
  | +--->
>-+ |   
>---+   
```

This part is hard:
```
idSM :: SM () a a

twoSM :: SM () a (a,a)

aSM :: SM () (Int,Int) (Int, Int)

bSM = aSM >>> (idSM &&& twoSM)
```

We should be able to figure out that the size of `a` is two. So it depends on how to connect SMs, and it is what the HM system does.


## References

[The Glorious Glasgow Haskell Compilation System User's Guide, Version 7.10.3](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)
  * [7. GHC Language Features](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghc-language-features.html)
