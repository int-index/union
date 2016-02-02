{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
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

data Union (f :: u -> *) (as :: [u]) where
  Union :: Either (Union f as) (f a) -> Union f (a ': as)

unionToEither :: Union f (a ': as) -> Either (Union f as) (f a)
unionToEither (Union e) = e

union :: (Union f as -> c) -> (f a -> c) -> Union f (a ': as) -> c
union onLeft onRight = either onLeft onRight . unionToEither

absurdUnion :: Union f '[] -> a
absurdUnion = \case{}

class LiftUnion a as where
  liftUnion :: f a -> Union f as

instance {-# OVERLAPPING #-} LiftUnion a (a ': as) where
  liftUnion = Union . Right

instance LiftUnion a as => LiftUnion a (b ': as) where
  liftUnion = Union . Left . liftUnion

class ReUnion as bs where
  reUnion :: Union f as -> Union f bs

instance ReUnion '[] bs where
  reUnion = absurdUnion

instance (LiftUnion a bs, ReUnion as bs) => ReUnion (a ': as) bs where
  reUnion = union reUnion liftUnion

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
    fromException sE
       =  fmap (liftUnion . Identity) matchA
      <|> fmap (Union . Left) matchU
      where
        matchA :: Maybe a
        matchU :: Maybe (Union f as)
        matchA = fromException sE
        matchU = fromException sE
