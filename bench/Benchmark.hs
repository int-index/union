{-# LANGUAGE DataKinds #-}
module Main (main) where

import Control.Lens
import Criterion.Main
import Data.Proxy
import Data.Union

union1 :: OpenUnion '[(), Proxy 0, Proxy 1]
union1 = openUnion # ()
{-# NOINLINE union1 #-}

union3 :: OpenUnion '[Proxy 0, Proxy 1, ()]
union3 = openUnion # ()
{-# NOINLINE union3 #-}

union7 :: OpenUnion
  '[ Proxy 0, Proxy 1, Proxy 2, Proxy 3
   , Proxy 4 , Proxy 5, Proxy 6, () ]
union7 = openUnion # ()
{-# NOINLINE union7 #-}

main :: IO ()
main = do
  defaultMain
    [ bench "openUnion matching 1st" $
        whnf (\a -> a ^? openUnion :: Maybe ()) union1
    , bench "openUnion matching 3rd" $
        whnf (\a -> a ^? openUnion :: Maybe ()) union3
    , bench "openUnion matching 7th" $
        whnf (\a -> a ^? openUnion :: Maybe ()) union7
    ]
