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

type OpenUnion12 = OpenUnion
  '[ Proxy 0, Proxy 1, Proxy 2, Proxy 3
   , Proxy 4, Proxy 5, Proxy 6, Proxy 7
   , Proxy 8, Proxy 9, Proxy 10, () ]

union12 :: OpenUnion12
union12 = openUnion # ()
{-# NOINLINE union12 #-}

either12 ::
  Either (Proxy 0)  (
  Either (Proxy 1)  (
  Either (Proxy 2)  (
  Either (Proxy 3)  (
  Either (Proxy 4)  (
  Either (Proxy 5)  (
  Either (Proxy 6)  (
  Either (Proxy 7)  (
  Either (Proxy 8)  (
  Either (Proxy 9)  (
  Either (Proxy 10) (
      ()
  )))))))))))
either12 =
  ( Right . Right . Right . Right
  . Right . Right . Right . Right
  . Right . Right . Right ) ()
{-# NOINLINE either12 #-}

main :: IO ()
main = do
  defaultMain
    [ bench "openUnion matching 1st" $
        whnf (\a -> a ^? openUnion :: Maybe ()) union1
    , bench "openUnion matching 3rd" $
        whnf (\a -> a ^? openUnion :: Maybe ()) union3
    , bench "openUnion matching 12th" $
        whnf (\a -> a ^? openUnion :: Maybe ()) union12
    , bench "nested either matching 12th" $
        whnf (\a -> a ^? _Right . _Right . _Right . _Right
                       . _Right . _Right . _Right . _Right
                       . _Right . _Right . _Right :: Maybe ()) either12
    , bench "openUnion constructing 12th" $
        whnf (\a -> openUnion # a :: OpenUnion12) ()
    , bench "dyn constructing" $
        whnf toDyn ()
    ]
