# Union

[![Build Status](https://github.com/int-index/union/workflows/CI/badge.svg)](https://github.com/int-index/union/actions)
[![Hackage](https://img.shields.io/hackage/v/union.svg)](https://hackage.haskell.org/package/union)

Extensible type-safe unions.

```
ghci> let a = openUnion # (5 :: Int) :: OpenUnion '[Bool, Int]

ghci> a ^? openUnion :: Maybe Int
Just 5

ghci> a ^? openUnion :: Maybe Bool
Nothing

ghci> a ^? openUnion :: Maybe Char
<interactive>:8:6:
    No instance for (UElem Char '[] (RIndex Char '[]))
      arising from a use of ‘openUnion’
    In the second argument of ‘(^?)’, namely ‘openUnion’
    In the expression: a ^? openUnion :: Maybe Char
    In an equation for ‘it’: it = a ^? openUnion :: Maybe Char
```
