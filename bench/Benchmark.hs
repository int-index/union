{-# OPTIONS -Wno-missing-signatures #-}
{-# LANGUAGE NoListTuplePuns #-}
module Main (main) where

import Control.Lens ((#), (^?), _Right)
import Control.DeepSeq (NFData(rnf), rwhnf)
import Criterion.Main (whnf, nf, bench, defaultMain)
import Data.Dynamic (toDyn)
import Data.Union (OpenUnion, openUnion)
import Data.Proxy (Proxy)

data Unit = MkUnit

instance NFData Unit where
  rnf = rwhnf

union1 :: OpenUnion [Unit, Proxy 0, Proxy 1]
union1 = openUnion # MkUnit
{-# NOINLINE union1 #-}

union3 :: OpenUnion [Proxy 0, Proxy 1, Unit]
union3 = openUnion # MkUnit
{-# NOINLINE union3 #-}

type OpenUnion12 = OpenUnion
  [ Proxy 0, Proxy 1, Proxy 2, Proxy 3
  , Proxy 4, Proxy 5, Proxy 6, Proxy 7
  , Proxy 8, Proxy 9, Proxy 10, Unit ]

union12 :: OpenUnion12
union12 = openUnion # MkUnit
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
      Unit
  )))))))))))
either12 =
  ( Right . Right . Right . Right
  . Right . Right . Right . Right
  . Right . Right . Right ) MkUnit
{-# NOINLINE either12 #-}

main = do
  defaultMain
    [ bench "openUnion matching 1st" $
        whnf (\a -> a ^? openUnion :: Maybe Unit) union1
    , bench "openUnion matching 3rd" $
        whnf (\a -> a ^? openUnion :: Maybe Unit) union3
    , bench "openUnion matching 12th" $
        whnf (\a -> a ^? openUnion :: Maybe Unit) union12
    , bench "nested either matching 12th" $
        whnf (\a -> a ^? _Right . _Right . _Right . _Right
                       . _Right . _Right . _Right . _Right
                       . _Right . _Right . _Right :: Maybe Unit) either12
    , bench "openUnion constructing 1st" $
        nf (\a -> openUnion # a :: OpenUnion [Unit]) MkUnit
    , bench "openUnion constructing 12th" $
        nf (\a -> openUnion # a :: OpenUnion12) MkUnit
    , bench "dyn constructing" $
        whnf toDyn MkUnit
    ]
