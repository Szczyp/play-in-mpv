{-# LANGUAGE RankNTypes #-}

module Preludium
  ( (>>)
  , (<<)
  , (|*>)
  , (<*|)
  , module X
  ) where

import ClassyPrelude              as X hiding (($), (<$>), (<|), (>>))
import Control.Error.Util         as X hiding (bool, hush, note, tryIO)
import Control.Lens               as X hiding
    ( Index
    , cons
    , index
    , snoc
    , uncons
    , unsnoc
    , (&)
    , (<&>)
    , (<.>)
    , (<|)
    , (??)
    , (|>)
    )
import Control.Monad.Morph        as X
import Data.EitherR               as X (fmapLT)
import Flow                       as X hiding ((.>), (<.))
import Protolude                  as X (hush, note, toS, toSL)

import qualified ClassyPrelude
import qualified Control.Lens  as Lens
import qualified Flow

(>>) :: forall a b c. (a -> b) -> (b -> c) -> a -> c
(>>) = (Flow..>)

(<<) :: forall b c a. (b -> c) -> (a -> b) -> a -> c
(<<) = (Flow.<.)
infixr 9 <<

(|*>) :: Functor f => f a -> (a -> b) -> f b
(|*>) = (Lens.<&>)
infixl 1 |*>

(<*|) :: Functor f => (a -> b) -> f a -> f b
(<*|) = (ClassyPrelude.<$>)
infixl 4 <*|
