{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Union where

import Control.Applicative
import Control.Exception
import Data.Function
import Data.Functor.Identity
import Data.Typeable
import Data.Vinyl.TypeLevel
import Control.Lens

data Union (f :: u -> *) (as :: [u]) where
  Union :: !(Either (Union f as) (f a)) -> Union f (a ': as)

unionToEither :: Union f (a ': as) -> Either (Union f as) (f a)
unionToEither (Union e) = e

_Union :: Iso' (Union f (a ': as)) (Either (Union f as) (f a))
_Union = iso unionToEither Union

union :: (Union f as -> c) -> (f a -> c) -> Union f (a ': as) -> c
union onLeft onRight = either onLeft onRight . unionToEither

absurdUnion :: Union f '[] -> a
absurdUnion = \case{}

uprismR :: Prism' (Union f (a ': as)) (f a)
uprismR = _Union . _Right

uprismL :: Prism' (Union f (a ': as)) (Union f as)
uprismL = _Union . _Left

class i ~ RIndex a as => UElem (a :: u) (as :: [u]) (i :: Nat) where
  uprism :: Prism' (Union f as) (f a)

instance UElem a (a ': as) 'Z where
  uprism = uprismR

instance
    ( RIndex a (b ': as) ~ 'S i
    , UElem a as i
    ) => UElem a (b ': as) ('S i)
  where
    uprism = uprismL . uprism

class is ~ RImage as bs => USubset (as :: [u]) (bs :: [u]) is where
  usubset :: Prism' (Union f bs) (Union f as)

instance USubset '[] bs '[] where
  usubset = prism absurdUnion Left

instance
    ( UElem a bs i
    , USubset as bs is
    ) => USubset (a ': as) bs (i ': is) where
  usubset = prism
    (union (review usubset) (review uprism))
    (\ubs -> maybe (Left ubs) Right
           $ preview (uprism  . re uprismR) ubs
         <|> preview (usubset . re uprismL) ubs)

instance Show (Union f '[]) where
  showsPrec _ = absurdUnion

instance
    ( Show (f a)
    , Show (Union f as)
    ) => Show (Union f (a ': as))
  where
    showsPrec n = showsPrec n . unionToEither

instance Eq (Union f '[]) where
  (==) = absurdUnion

instance
    ( Eq (f a)
    , Eq (Union f as)
    ) => Eq (Union f (a ': as))
  where
    (==) = (==) `on` unionToEither

instance Ord (Union f '[]) where
  compare = absurdUnion

instance
    ( Ord (f a)
    , Ord (Union f as)
    ) => Ord (Union f (a ': as))
  where
    compare = compare `on` unionToEither

instance f ~ Identity => Exception (Union f '[])

instance
    ( f ~ Identity
    , Exception a
    , Typeable as
    , Exception (Union f as)
    ) => Exception (Union f (a ': as))
  where
    toException = union toException (toException . runIdentity)
    fromException sE = matchR <|> matchL
      where
        matchR = review uprismR . Identity <$> fromException sE
        matchL = review uprismL <$> fromException sE
