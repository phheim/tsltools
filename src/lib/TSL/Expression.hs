-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Expression
-- Maintainer  :  Felix Klein
--
-- Data types to store expressions and some helper functions.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

module TSL.Expression
  ( Constant
  , ExprId
  , Expression
  , Expr(..)
  , Expr'(..)
  , ExprPos(..)
  , SrcPos(..)
  , subExpressions
  , applySub
  , prExpr
  ) where

-----------------------------------------------------------------------------

-- | A constant is represented by an integer

type Constant = Int

-----------------------------------------------------------------------------

-- | Expressions are identified by their unique expression id

type ExprId = Int

-----------------------------------------------------------------------------

-- | Each expression consists of two parts, the main expression
-- itself, i.e., the basic term or an operation, and the position of
-- the expression in the source code. The type $a$ specifies the
-- representation of an identifier.

data Expr a =
  Expr
  { expr :: Expr' a
  , exprId :: ExprId
  , srcPos :: ExprPos
  } deriving (Show,Eq)

-----------------------------------------------------------------------------

instance Functor Expr where
  fmap f e@Expr{..} =
    e { expr = fmap f expr }

-----------------------------------------------------------------------------

-- | We use the type @Expression@ as a shortcut for expressions, where
-- identifiers are denoted by integers.

type Expression = Expr Int

-----------------------------------------------------------------------------

-- | An expression is either a basic term or the composition of
-- multiple sub-expressions using an operator. To obtain a stepwise
-- refinement of the parsed data, an expression does not need to be
-- type consistent ia the first place, e.g., techniqually we could add
-- to boolean expressions.  Such behaviour is ruled out later during
-- the type analysis.

data Expr' a =
    BaseWild
  | BaseTrue
  | BaseFalse
  | BaseOtherwise
  | BaseCon Constant

  | BaseId a
  | BaseConFn a
  | BaseUpd (Expr a) a
  | BaseFn (Expr a) (Expr a)

  | NumSMin (Expr a)
  | NumSMax (Expr a)
  | NumSSize (Expr a)
  | NumPlus (Expr a) (Expr a)
  | NumRPlus [Expr a] (Expr a)
  | NumMinus (Expr a) (Expr a)
  | NumMul (Expr a) (Expr a)
  | NumRMul [Expr a] (Expr a)
  | NumDiv (Expr a) (Expr a)
  | NumMod (Expr a) (Expr a)

  | SetExplicit [Expr a]
  | SetRange (Expr a) (Expr a) (Expr a)
  | SetCup (Expr a) (Expr a)
  | SetRCup [Expr a] (Expr a)
  | SetCap (Expr a) (Expr a)
  | SetRCap [Expr a] (Expr a)
  | SetMinus (Expr a) (Expr a)

  | BlnEQ (Expr a) (Expr a)
  | BlnNEQ (Expr a) (Expr a)
  | BlnGE (Expr a) (Expr a)
  | BlnGEQ (Expr a) (Expr a)
  | BlnLE (Expr a) (Expr a)
  | BlnLEQ (Expr a) (Expr a)
  | BlnElem (Expr a) (Expr a)
  | BlnNot (Expr a)
  | BlnOr (Expr a) (Expr a)
  | BlnROr [Expr a] (Expr a)
  | BlnAnd (Expr a) (Expr a)
  | BlnRAnd [Expr a] (Expr a)
  | BlnImpl (Expr a) (Expr a)
  | BlnEquiv (Expr a) (Expr a)

  | TslNext (Expr a)
  | TslRNext (Expr a) (Expr a)
  | TslPrevious (Expr a)
  | TslRPrevious (Expr a) (Expr a)
  | TslGlobally (Expr a)
  | TslRGlobally (Expr a) (Expr a)
  | TslFinally (Expr a)
  | TslRFinally (Expr a) (Expr a)
  | TslHistorically (Expr a)
  | TslRHistorically (Expr a) (Expr a)
  | TslOnce (Expr a)
  | TslROnce (Expr a) (Expr a)
  | TslUntil (Expr a) (Expr a)
  | TslWeak (Expr a) (Expr a)
  | TslAsSoonAs (Expr a) (Expr a)
  | TslRelease (Expr a) (Expr a)
  | TslSince (Expr a) (Expr a)
  | TslTriggered (Expr a) (Expr a)

  | Colon (Expr a) (Expr a)
  | Pattern (Expr a) (Expr a)
  deriving (Show, Eq)

-----------------------------------------------------------------------------

instance Functor Expr' where
  fmap f = \case
    BaseWild             -> BaseWild
    BaseTrue             -> BaseTrue
    BaseFalse            -> BaseFalse
    BaseOtherwise        -> BaseOtherwise
    BaseCon c            -> BaseCon c
    BaseId i             -> BaseId $ f i
    BaseConFn i          -> BaseConFn $ f i
    BaseUpd x i          -> BaseUpd (fmap f x) $ f i
    BaseFn  x y          -> BaseFn (fmap f x) $ fmap f y
    NumSMin x            -> NumSMin $ fmap f x
    NumSMax x            -> NumSMax $ fmap f x
    NumSSize x           -> NumSSize $ fmap f x
    NumPlus x y          -> NumPlus (fmap f x) $ fmap f y
    NumRPlus xs x        -> NumRPlus (map (fmap f) xs) $ fmap f x
    NumMinus x y         -> NumMinus (fmap f x) $ fmap f y
    NumMul x y           -> NumMul (fmap f x) $ fmap f y
    NumRMul xs x         -> NumRMul (map (fmap f) xs) $ fmap f x
    NumDiv x y           -> NumDiv (fmap f x) $ fmap f y
    NumMod x y           -> NumMod (fmap f x) $ fmap f y
    SetExplicit xs       -> SetExplicit $ map (fmap f) xs
    SetRange x y z       -> SetRange (fmap f x) (fmap f y) $ fmap f z
    SetCup  x y          -> SetCup (fmap f x) $ fmap f y
    SetRCup xs x         -> SetRCup (map (fmap f) xs) $ fmap f x
    SetCap x y           -> SetCap (fmap f x) $ fmap f y
    SetRCap xs x         -> SetRCap (map (fmap f) xs) $ fmap f x
    SetMinus x y         -> SetMinus (fmap f x) $ fmap f y
    BlnEQ x y            -> BlnEQ (fmap f x) $ fmap f y
    BlnNEQ x y           -> BlnNEQ (fmap f x) $ fmap f y
    BlnGE x y            -> BlnGE (fmap f x) $ fmap f y
    BlnGEQ x y           -> BlnGEQ (fmap f x) $ fmap f y
    BlnLE x y            -> BlnLE (fmap f x) $ fmap f y
    BlnLEQ x y           -> BlnLEQ (fmap f x) $ fmap f y
    BlnElem x y          -> BlnElem (fmap f x) $ fmap f y
    BlnNot x             -> BlnNot $ fmap f x
    BlnOr x y            -> BlnOr (fmap f x) $ fmap f y
    BlnROr xs x          -> BlnROr (map (fmap f) xs) $ fmap f x
    BlnAnd x y           -> BlnAnd (fmap f x) $ fmap f y
    BlnRAnd xs x         -> BlnRAnd (map (fmap f) xs) $ fmap f x
    BlnImpl x y          -> BlnImpl (fmap f x) $ fmap f y
    BlnEquiv x y         -> BlnEquiv (fmap f x) $ fmap f y
    TslNext x            -> TslNext $ fmap f x
    TslRNext x y         -> TslRNext (fmap f x) $ fmap f y
    TslPrevious x        -> TslPrevious $ fmap f x
    TslRPrevious x y     -> TslRPrevious (fmap f x) $ fmap f y
    TslGlobally x        -> TslGlobally $ fmap f x
    TslRGlobally x y     -> TslRGlobally (fmap f x) $ fmap f y
    TslFinally x         -> TslFinally $ fmap f x
    TslRFinally x y      -> TslRFinally (fmap f x) $ fmap f y
    TslHistorically x    -> TslHistorically $ fmap f x
    TslRHistorically x y -> TslRHistorically (fmap f x) $ fmap f y
    TslOnce x            -> TslOnce $ fmap f x
    TslROnce x y         -> TslROnce (fmap f x) $ fmap f y
    TslUntil x y         -> TslUntil (fmap f x) $ fmap f y
    TslWeak x y          -> TslWeak (fmap f x) $ fmap f y
    TslAsSoonAs x y      -> TslAsSoonAs (fmap f x) $ fmap f y
    TslRelease x y       -> TslRelease (fmap f x) $ fmap f y
    TslSince x y         -> TslSince (fmap f x) $ fmap f y
    TslTriggered x y     -> TslTriggered (fmap f x) $ fmap f y
    Colon x y            -> Colon (fmap f x) $ fmap f y
    Pattern x y          -> Pattern (fmap f x) $ fmap f y

-----------------------------------------------------------------------------

-- | The position of an expression is denoted by its starting position
-- and ending position in the source code.

data ExprPos =
  ExprPos
  { srcBegin :: SrcPos
  , srcEnd :: SrcPos
  , srcPath :: Maybe FilePath
  } deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

-- | A position in the source code is uniquely identified by its line and
-- column.

data SrcPos =
  SrcPos
  { srcLine :: Int
  , srcColumn :: Int
  } deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

-- | Returns all direct sub-formulas of the given formula, i.e., the
-- formulas that appear under the first operator. If the given formula
-- is a basic term, an empty list is returned.

subExpressions
  :: Expr a -> [Expr a]

subExpressions e = case expr e of
  BaseWild             -> []
  BaseTrue             -> []
  BaseFalse            -> []
  BaseCon _            -> []
  BaseId _             -> []
  BaseConFn _          -> []
  BaseOtherwise        -> []
  BaseUpd x _          -> [x]
  NumSMin x            -> [x]
  NumSMax x            -> [x]
  NumSSize x           -> [x]
  BlnNot x             -> [x]
  TslNext x            -> [x]
  TslPrevious x        -> [x]
  TslGlobally x        -> [x]
  TslFinally x         -> [x]
  TslHistorically x    -> [x]
  TslOnce x            -> [x]
  BaseFn x y           -> [x,y]
  NumPlus x y          -> [x,y]
  NumMinus x y         -> [x,y]
  NumMul x y           -> [x,y]
  NumDiv x y           -> [x,y]
  NumMod x y           -> [x,y]
  SetCup x y           -> [x,y]
  SetCap x y           -> [x,y]
  SetMinus x y         -> [x,y]
  BlnEQ x y            -> [x,y]
  BlnNEQ x y           -> [x,y]
  BlnGE x y            -> [x,y]
  BlnGEQ x y           -> [x,y]
  BlnLE x y            -> [x,y]
  BlnLEQ x y           -> [x,y]
  BlnElem x y          -> [x,y]
  BlnOr x y            -> [x,y]
  BlnAnd x y           -> [x,y]
  BlnImpl x y          -> [x,y]
  BlnEquiv x y         -> [x,y]
  TslRNext x y         -> [x,y]
  TslRPrevious x y     -> [x,y]
  TslRGlobally x y     -> [x,y]
  TslRFinally x y      -> [x,y]
  TslRHistorically x y -> [x,y]
  TslROnce x y         -> [x,y]
  TslUntil x y         -> [x,y]
  TslWeak x y          -> [x,y]
  TslAsSoonAs x y      -> [x,y]
  TslRelease x y       -> [x,y]
  TslSince x y         -> [x,y]
  TslTriggered x y     -> [x,y]
  Colon x y            -> [x,y]
  Pattern x y          -> [x,y]
  SetRange x y z       -> [x,y,z]
  SetExplicit xs       -> xs
  NumRPlus xs x        -> x:xs
  NumRMul xs x         -> x:xs
  SetRCup xs x         -> x:xs
  SetRCap xs x         -> x:xs
  BlnROr xs x          -> x:xs
  BlnRAnd xs x         -> x:xs

-----------------------------------------------------------------------------

-- | Applies function 'f' to the fist level sup-expressions of 'e'.

applySub
  :: (Expr a -> Expr a) -> Expr a -> Expr a

applySub f e =
  let
    e' = case expr e of
      BaseWild             -> BaseWild
      BaseTrue             -> BaseTrue
      BaseFalse            -> BaseFalse
      BaseCon i            -> BaseCon i
      BaseOtherwise        -> BaseOtherwise
      BaseId i             -> BaseId i
      BaseConFn i          -> BaseConFn i
      BaseUpd x i          -> BaseUpd (f x) i
      NumSMin x            -> NumSMin $ f x
      NumSMax x            -> NumSMax $ f x
      NumSSize x           -> NumSSize $ f x
      BlnNot x             -> BlnNot $ f x
      TslNext x            -> TslNext $ f x
      TslPrevious x        -> TslPrevious $ f x
      TslGlobally x        -> TslGlobally $ f x
      TslFinally x         -> TslFinally  $ f x
      TslHistorically x    -> TslHistorically $ f x
      TslOnce x            -> TslOnce x
      BaseFn x y           -> BaseFn (f x) (f y)
      NumPlus x y          -> NumPlus (f x) (f y)
      NumMinus x y         -> NumMinus (f x) (f y)
      NumMul x y           -> NumMul (f x) (f y)
      NumDiv x y           -> NumDiv (f x) (f y)
      NumMod x y           -> NumMod (f x) (f y)
      SetCup x y           -> SetCup (f x) (f y)
      SetCap x y           -> SetCap (f x) (f y)
      SetMinus x y         -> SetMinus (f x) (f y)
      BlnEQ x y            -> BlnEQ (f x) (f y)
      BlnNEQ x y           -> BlnNEQ (f x) (f y)
      BlnGE x y            -> BlnGE (f x) (f y)
      BlnGEQ x y           -> BlnGEQ (f x) (f y)
      BlnLE x y            -> BlnLE (f x) (f y)
      BlnLEQ x y           -> BlnLEQ (f x) (f y)
      BlnElem x y          -> BlnElem (f x) (f y)
      BlnOr x y            -> BlnOr (f x) (f y)
      BlnAnd x y           -> BlnAnd (f x) (f y)
      BlnImpl x y          -> BlnImpl (f x) (f y)
      BlnEquiv x y         -> BlnEquiv (f x) (f y)
      TslRNext x y         -> TslRNext (f x) (f y)
      TslRPrevious x y     -> TslRPrevious (f x) (f y)
      TslRGlobally x y     -> TslRGlobally (f x) (f y)
      TslRFinally x y      -> TslRFinally (f x) (f y)
      TslRHistorically x y -> TslRHistorically (f x) (f y)
      TslROnce x y         -> TslROnce (f x) (f y)
      TslUntil x y         -> TslUntil (f x) (f y)
      TslWeak x y          -> TslWeak (f x) (f y)
      TslAsSoonAs x y      -> TslAsSoonAs (f x) (f y)
      TslRelease x y       -> TslRelease (f x) (f y)
      TslSince x y         -> TslSince (f x) (f y)
      TslTriggered x y     -> TslTriggered (f x) (f y)
      Colon x y            -> Colon (f x) (f y)
      Pattern x y          -> Pattern (f x) (f y)
      SetRange x y z       -> SetRange (f x) (f y) (f z)
      SetExplicit xs       -> SetExplicit $ map f xs
      NumRPlus xs x        -> NumRPlus (map f xs) (f x)
      NumRMul xs x         -> NumRMul (map f xs) (f x)
      SetRCup xs x         -> SetRCup (map f xs) (f x)
      SetRCap xs x         -> SetRCap (map f xs) (f x)
      BlnROr xs x          -> BlnROr (map f xs) (f x)
      BlnRAnd xs x         -> BlnRAnd (map f xs) (f x)
  in
    e { expr = e' }

-----------------------------------------------------------------------------

-- | Some debugging function to give a more readable version of the
-- expression.  In constrast to @show@, this function drops all
-- position information in the resulting output (for debugging
-- purposes only).

prExpr
  :: (Int -> String) -> Expr Int -> String

prExpr f e = case expr e of
  BaseWild             -> "WILD"
  BaseTrue             -> "TRUE"
  BaseFalse            -> "FALSE"
  BaseOtherwise        -> "OTHERWISE"
  BaseCon x            -> "(CON " ++ show x ++ ")"
  BaseId x             -> "(ID " ++ f x ++ ")"
  BaseUpd x y          -> "(UPD " ++ f y ++ " <- " ++ prExpr f x ++ ")"
  BaseConFn x          -> "(FN " ++ f x ++ ")"
  BaseFn x y           -> "(FN " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  NumSMin x            -> "(MIN " ++ prExpr f x ++ ")"
  NumSMax x            -> "(MAX " ++ prExpr f x ++ ")"
  NumSSize x           -> "(SIZE " ++ prExpr f x ++ ")"
  BlnNot x             -> "(NOT " ++ prExpr f x ++ ")"
  TslNext x            -> "(X " ++ prExpr f x ++ ")"
  TslPrevious x        -> "(Y " ++ prExpr f x ++ ")"
  TslGlobally x        -> "(G " ++ prExpr f x ++ ")"
  TslFinally x         -> "(F " ++ prExpr f x ++ ")"
  TslHistorically x    -> "(H " ++ prExpr f x ++ ")"
  TslOnce x            -> "(O " ++ prExpr f x ++ ")"
  NumPlus x y          -> "(PLUS " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  NumMinus x y         -> "(MINUS " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  NumMul x y           -> "(MUL " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  NumDiv x y           -> "(DIV " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  NumMod x y           -> "(MOD " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  SetCup x y           -> "(CUP " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  SetCap x y           -> "(CAP " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  SetMinus x y         -> "(DIFF " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  BlnEQ x y            -> "(EQ " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  BlnNEQ x y           -> "(NEQ " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  BlnGE x y            -> "(GE " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  BlnGEQ x y           -> "(GEQ " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  BlnLE x y            -> "(LE " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  BlnLEQ x y           -> "(LEQ " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  BlnElem x y          -> "(ELEM " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  BlnOr x y            -> "(OR " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  BlnAnd x y           -> "(AND " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  BlnImpl x y          -> "(IMPL " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  BlnEquiv x y         -> "(EQIV " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  TslRNext x y         -> "(X[" ++ prExpr f x ++ "] " ++ prExpr f y ++ ")"
  TslRPrevious x y     -> "(Y[" ++ prExpr f x ++ "] " ++ prExpr f y ++ ")"
  TslRGlobally x y     -> "(G[" ++ prExpr f x ++ "] " ++ prExpr f y ++ ")"
  TslRFinally x y      -> "(F[" ++ prExpr f x ++ "] " ++ prExpr f y ++ ")"
  TslRHistorically x y -> "(H[" ++ prExpr f x ++ "] " ++ prExpr f y ++ ")"
  TslROnce x y         -> "(O[" ++ prExpr f x ++ "] " ++ prExpr f y ++ ")"
  TslUntil x y         -> "(U " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  TslWeak x y          -> "(W " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  TslAsSoonAs x y      -> "(A " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  TslRelease x y       -> "(R " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  TslSince x y         -> "(S " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  TslTriggered x y     -> "(T " ++ prExpr f x ++ " " ++ prExpr f y ++ ")"
  Colon x y            -> prExpr f x ++ " : " ++ prExpr f y
  Pattern x y          -> prExpr f x ++ " ~ " ++ prExpr f y
  SetRange x y z       -> "(SR " ++ prExpr f x ++ " " ++ prExpr f y ++ " " ++ prExpr f z ++ ")"
  SetExplicit xs       -> "(SET " ++ concatMap (flip (++) " " . prExpr f) xs ++ ")"
  NumRPlus xs x        -> "(PLUS[" ++ concatMap (flip (++) " " . prExpr f) xs ++ "] " ++ prExpr f x ++ ")"
  NumRMul xs x         -> "(MUL[" ++ concatMap (flip (++) " " . prExpr f) xs ++ "] " ++ prExpr f x ++ ")"
  SetRCup xs x         -> "(CUP[" ++ concatMap (flip (++) " " . prExpr f) xs ++ "] " ++ prExpr f x ++ ")"
  SetRCap xs x         -> "(CAP[" ++ concatMap (flip (++) " " . prExpr f) xs ++ "] " ++ prExpr f x ++ ")"
  BlnROr xs x          -> "(OR[" ++ concatMap (flip (++) " " . prExpr f) xs ++ "] " ++ prExpr f x ++ ")"
  BlnRAnd xs x         -> "(AND[" ++ concatMap (flip (++) " " . prExpr f) xs ++ "] " ++ prExpr f x ++ ")"

-----------------------------------------------------------------------------
