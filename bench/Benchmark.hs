{-# LANGUAGE DataKinds #-}
module Main (main) where

import Control.Lens
import Criterion.Main
import Data.Dynamic
import Data.Union

union1 :: OpenUnion '[(), Proxy 0, Proxy 1]
union1 = openUnion # ()
{-# NOINLINE union1 #-}

union3 :: OpenUnion '[Proxy 0, Proxy 1, ()]
union3 = openUnion # ()
{-# NOINLINE union3 #-}

type OpenUnion7 = OpenUnion
  '[ Proxy 0, Proxy 1, Proxy 2, Proxy 3
   , Proxy 4 , Proxy 5, Proxy 6, () ]

union7 :: OpenUnion7
union7 = openUnion # ()
{-# NOINLINE union7 #-}

either7 ::
  Either (Proxy 0) (
  Either (Proxy 1) (
  Either (Proxy 2) (
  Either (Proxy 3) (
  Either (Proxy 4) (
  Either (Proxy 5) (
  Either (Proxy 6) (
      ()
  )))))))
either7 = (Right . Right . Right . Right . Right . Right . Right) ()
{-# NOINLINE either7 #-}

main :: IO ()
main = do
  defaultMain
    [ bench "openUnion matching 1st" $
        whnf (\a -> a ^? openUnion :: Maybe ()) union1
    , bench "openUnion matching 3rd" $
        whnf (\a -> a ^? openUnion :: Maybe ()) union3
    , bench "openUnion matching 7th" $
        whnf (\a -> a ^? openUnion :: Maybe ()) union7
    , bench "nested either matching 7th" $
        whnf (\a -> a ^? _Right . _Right . _Right
                       . _Right . _Right . _Right
                       . _Right :: Maybe ()) either7
    , bench "openUnion constructing 7th" $
        whnf (\a -> openUnion # a :: OpenUnion7) ()
    , bench "dyn constructing" $
        whnf toDyn ()
    ]
