# Development notes

## Ideas

### PartialTypeSignatures

```
{-# LANGUAGE PartialTypeSignatures #-}

f :: Int -> _ 
f x = x + 1
```

## References

[The Glorious Glasgow Haskell Compilation System User's Guide, Version 7.10.3](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)
  * [7. GHC Language Features](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghc-language-features.html)