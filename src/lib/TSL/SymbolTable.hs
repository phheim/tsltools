-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.SymbolTable
-- Maintainer  :  Felix Klein
--
-- Data type to store all identifier specific content.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

-----------------------------------------------------------------------------

module TSL.SymbolTable
  ( SymbolTable(..)
  , IdRec(..)
  , Kind(..)
  , symbolTable
  ) where

-----------------------------------------------------------------------------

import TSL.Types (ExprType(..))

import TSL.Expression (ExprPos(..))

import TSL.Binding (BoundExpr)

import Data.Array (Array, bounds, (!))

-----------------------------------------------------------------------------

type Id = Int

-----------------------------------------------------------------------------

data Kind =
  Input | Output | Constant | Predicate | Function | Internal
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

data SymbolTable =
  SymbolTable
    { symtable :: Array Id IdRec
    , stBounds :: (Id, Id)
    , stName :: Id -> String
    , stPos :: Id -> Maybe ExprPos
    , stArgs ::  Id -> [Id]
    , stBindings :: Id -> Maybe (BoundExpr Id)
    , stType :: Id -> ExprType
    , stDeps :: Id -> [Id]
    , stKind :: Id -> Kind
    }

-----------------------------------------------------------------------------

symbolTable
  :: Array Int IdRec -> SymbolTable

symbolTable a =
  SymbolTable
    { symtable = a
    , stBounds = bounds a
    , stName = idName . (a !)
    , stPos = idPos . (a !)
    , stArgs = idArgs . (a !)
    , stBindings = idBindings . (a !)
    , stType = idType . (a !)
    , stDeps = idDeps . (a !)
    , stKind = idKind . (a !)
    }

-----------------------------------------------------------------------------

-- | Data type representing a single entry in the symbol table.

data IdRec =
  IdRec
    { -- | The name of the identifier.
      idName :: String
    , -- | The type of the identifier.
      idType :: ExprType
    , -- | Categorization to distinguish between globally or locally
      -- bound indentifiers and functions, predicates, input or output
      -- signals.
      idKind :: Kind
    , -- | The list of identifiers, which have to be evaluated first
      -- to evaluate this identifier.
      idDeps :: [Id]
    , -- | The arguemnts, in case the identifier describes a function.
      idArgs :: [Id]
    , -- | The position of the identifer definition in the source file.
      idPos :: Maybe ExprPos
    , -- | The expression, the identifier is bound to.
      idBindings :: Maybe (BoundExpr Id)
    }

