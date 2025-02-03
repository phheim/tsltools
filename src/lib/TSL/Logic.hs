-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Logic
-- Maintainer  :  Felix Klein
--
-- TSL logic data type and utility functions.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module TSL.Logic
  ( Formula(..)
  , SignalTerm(..)
  , FunctionTerm(..)
  , PredicateTerm(..)
  ) where

-----------------------------------------------------------------------------

-- | Representation of TSL signal terms.

data SignalTerm a =
    Signal a
  | FunctionTerm (FunctionTerm a)
  | PredicateTerm (PredicateTerm a)
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

instance Functor SignalTerm where
  fmap f = \case
    Signal s        -> Signal $ f s
    FunctionTerm t  -> FunctionTerm $ fmap f t
    PredicateTerm t -> PredicateTerm $ fmap f t

instance Foldable SignalTerm where
  foldr f a = \case
    Signal s        -> f s a
    FunctionTerm t  -> foldr f a t
    PredicateTerm t -> foldr f a t

-----------------------------------------------------------------------------

-- | Representation of TSL function terms.

data FunctionTerm a =
    FunctionSymbol a
  | FApplied (FunctionTerm a) (SignalTerm a)
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

instance Functor FunctionTerm where
  fmap f = \case
    FunctionSymbol s -> FunctionSymbol $ f s
    FApplied t t'    -> FApplied (fmap f t) (fmap f t')

instance Foldable FunctionTerm where
  foldr f a = \case
    FunctionSymbol s -> f s a
    FApplied t t'    -> foldr f (foldr f a t) t'

-----------------------------------------------------------------------------

-- | Represenation of TSL predicate terms.

data PredicateTerm a =
    BooleanTrue
  | BooleanFalse
  | BooleanInput a
  | PredicateSymbol a
  | PApplied (PredicateTerm a) (SignalTerm a)
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

instance Functor PredicateTerm where
  fmap f = \case
    BooleanTrue       -> BooleanTrue
    BooleanFalse      -> BooleanFalse
    BooleanInput s    -> BooleanInput $ f s
    PredicateSymbol s -> PredicateSymbol $ f s
    PApplied t t'     -> PApplied (fmap f t) (fmap f t')

instance Foldable PredicateTerm where
  foldr f a = \case
    BooleanTrue       -> a
    BooleanFalse      -> a
    BooleanInput s    -> f s a
    PredicateSymbol s -> f s a
    PApplied t t'     -> foldr f (foldr f a t) t'

-----------------------------------------------------------------------------

data Formula a =
    TTrue
  | FFalse
  | Check (PredicateTerm a)
  | Update a (SignalTerm a)
  | Not (Formula a)
  | Implies (Formula a) (Formula a)
  | Equiv (Formula a) (Formula a)
  | And [Formula a]
  | Or [Formula a]
  | Next (Formula a)
  | Previous (Formula a)
  | Globally (Formula a)
  | Finally (Formula a)
  | Historically (Formula a)
  | Once (Formula a)
  | Until (Formula a) (Formula a)
  | Release (Formula a) (Formula a)
  | Weak (Formula a) (Formula a)
  | Since (Formula a) (Formula a)
  | Triggered (Formula a) (Formula a)
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

instance Functor Formula where
  fmap f = \case
    TTrue          -> TTrue
    FFalse         -> FFalse
    Check t        -> Check $ fmap f t
    Update s t     -> Update (f s) $ fmap f t
    Not x          -> Not $ fmap f x
    Implies x y    -> Implies (fmap f x) $ fmap f y
    Equiv x y      -> Equiv (fmap f x) $ fmap f y
    And xs         -> And $ fmap (fmap f) xs
    Or xs          -> Or $ fmap (fmap f) xs
    Next x         -> Next $ fmap f x
    Previous x     -> Previous $ fmap f x
    Globally x     -> Globally $ fmap f x
    Finally x      -> Finally $ fmap f x
    Historically x -> Historically $ fmap f x
    Once x         -> Once $ fmap f x
    Until x y      -> Until (fmap f x) $ fmap f y
    Release x y    -> Release (fmap f x) $ fmap f y
    Weak x y       -> Weak (fmap f x) $ fmap f y
    Since x y      -> Since (fmap f x) $ fmap f y
    Triggered x y  -> Triggered (fmap f x) $ fmap f y

instance Foldable Formula where
  foldr f a = \case
    TTrue          -> a
    FFalse         -> a
    Check t        -> foldr f a t
    Update s t     -> foldr f (f s a) t
    Not x          -> foldr f a x
    Implies x y    -> foldr f (foldr f a x) y
    Equiv x y      -> foldr f (foldr f a x) y
    And xs         -> foldr (flip $ foldr f) a xs
    Or xs          -> foldr (flip $ foldr f) a xs
    Next x         -> foldr f a x
    Previous x     -> foldr f a x
    Globally x     -> foldr f a x
    Finally x      -> foldr f a x
    Historically x -> foldr f a x
    Once x         -> foldr f a x
    Until x y      -> foldr f (foldr f a x) y
    Release x y    -> foldr f (foldr f a x) y
    Weak x y       -> foldr f (foldr f a x) y
    Since x y      -> foldr f (foldr f a x) y
    Triggered x y  -> foldr f (foldr f a x) y

