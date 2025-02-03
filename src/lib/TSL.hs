-----------------------------------------------------------------------------
-- |
-- Module      :  TSL
-- Maintainer  :  Felix Klein
--                Philippe Heim
--                Gideon Geier
--                Marvin Stenger
--
--
-- TSL tools library interface.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module TSL
  ( Formula(..)
  , SignalTerm(..)
  , FunctionTerm(..)
  , PredicateTerm(..)
  , Specification(..)
  , fromTSL
  , SymbolTable
  , stName
  , Error
  ) where

-----------------------------------------------------------------------------

import TSL.Logic
  ( Formula(..)
  , FunctionTerm(..)
  , PredicateTerm(..)
  , SignalTerm(..)
  )

import TSL.Error (Error)

import TSL.SymbolTable (SymbolTable(stName))

import TSL.Specification (Specification(..))

import TSL.Reader (fromTSL)

-----------------------------------------------------------------------------
