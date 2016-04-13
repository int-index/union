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

{- |

Extensible type-safe unions.

-}

module Data.Union
  ( Union(..)
  , union
  , absurdUnion
  , umap
  , _This
  , _That
  , UElem(..)
  , USubset(..)
  , OpenUnion
  , openUnion
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Lens
  ( Prism
  , prism
  , Prism'
  , prism'
  , iso
  , re
  , review
  , preview )
import Data.Functor.Identity
import Data.Typeable
import Data.Vinyl.TypeLevel

-- | A union is parameterized by a universe @u@, an interpretation @f@
-- and a list of labels @as@. The labels of the union are given by
-- inhabitants of the kind @u@; the type of values at any label @a ::
-- u@ is given by its interpretation @f a :: *@.
data Union (f :: u -> *) (as :: [u]) where
  This :: !(f a) -> Union f (a ': as)
  That :: !(Union f as) -> Union f (a ': as)

-- | Case analysis for unions.
union :: (Union f as -> c) -> (f a -> c) -> Union f (a ': as) -> c
union onThat onThis = \case
  This a -> onThis a
  That u -> onThat u

-- | Since a union with an empty list of labels is uninhabited, we
-- can recover any type from it.
absurdUnion :: Union f '[] -> a
absurdUnion = \case{}

umap :: (forall a . f a -> g a) -> Union f as -> Union g as
umap f = \case
  This a -> This (f a)
  That u -> That (umap f u)

_This :: Prism (Union f (a ': as)) (Union f (b ': as)) (f a) (f b)
_This = prism This (union (Left . That) Right)
{-# INLINE _This #-}

_That :: Prism (Union f (a ': as)) (Union f (a ': bs)) (Union f as) (Union f bs)
_That = prism That (union Right (Left . This))
{-# INLINE _That #-}

class i ~ RIndex a as => UElem (a :: u) (as :: [u]) (i :: Nat) where
  uprism :: Prism' (Union f as) (f a)

instance UElem a (a ': as) 'Z where
  uprism = _This
  {-# INLINE uprism #-}

instance
    ( RIndex a (b ': as) ~ 'S i
    , UElem a as i
    ) => UElem a (b ': as) ('S i)
  where
    uprism = _That . uprism
    {-# INLINE uprism #-}

class is ~ RImage as bs => USubset (as :: [u]) (bs :: [u]) is where
  usubset :: Prism' (Union f bs) (Union f as)

instance USubset '[] bs '[] where
  usubset = prism absurdUnion Left

instance
    ( UElem a bs i
    , USubset as bs is
    ) => USubset (a ': as) bs (i ': is) where
  usubset = prism'
    (union (review usubset) (review uprism))
    (\ubs -> preview (uprism  . re _This) ubs
         <|> preview (usubset . re _That) ubs)

type OpenUnion = Union Identity

openUnion :: forall a as . UElem a as (RIndex a as) => Prism' (OpenUnion as) a
openUnion = uprism . iso runIdentity Identity
{-# INLINE openUnion #-}

instance NFData (Union f '[]) where
  rnf = absurdUnion

instance
    ( NFData (f a)
    , NFData (Union f as)
    ) => NFData (Union f (a ': as))
  where
    rnf = union rnf rnf

instance Show (Union f '[]) where
  showsPrec _ = absurdUnion

instance
    ( Show (f a)
    , Show (Union f as)
    ) => Show (Union f (a ': as))
  where
    showsPrec n = union (showsPrec n) (showsPrec n)

instance Eq (Union f '[]) where
  (==) = absurdUnion

instance
    ( Eq (f a)
    , Eq (Union f as)
    ) => Eq (Union f (a ': as))
  where
    This a1 == This a2 = a1 == a2
    That u1 == That u2 = u1 == u2
    _       == _       = False

instance Ord (Union f '[]) where
  compare = absurdUnion

instance
    ( Ord (f a)
    , Ord (Union f as)
    ) => Ord (Union f (a ': as))
  where
    compare (This a1) (This a2) = compare a1 a2
    compare (That u1) (That u2) = compare u1 u2
    compare (This _)  (That _)  = LT
    compare (That _)  (This _)  = GT

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
        matchR = This . Identity <$> fromException sE
        matchL = That <$> fromException sE
