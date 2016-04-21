{-# LANGUAGE RankNTypes #-}
module Data.Union.Prism
  ( Prism
  , prism
  , Prism'
  , prism'
  , iso
  , review
  , preview
  ) where

import Control.Applicative
import Data.Functor.Identity
import Data.Monoid
import Data.Profunctor
import Data.Coerce
import Data.Tagged

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Prism' s a = Prism s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

review :: Prism' t b -> b -> t
review p = coerce . p . Tagged . Identity
{-# INLINE review #-}

preview :: Prism' s a -> s -> Maybe a
preview l = coerce . l (Const . First . Just)
{-# INLINE preview #-}
