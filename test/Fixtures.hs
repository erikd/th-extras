{-# LANGUAGE CPP, GADTs, KindSignatures, PolyKinds, TemplateHaskell #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE RankNTypes, StandaloneKindSignatures #-}
#endif
#if __GLASGOW_HASKELL__ >= 906
{-# LANGUAGE TypeData #-}
#endif

-- | Declarations to reify in compile-time tests, and helpers for splicing in
-- the results of running library functions on them.
module Fixtures
  ( arityCases
  , KindSig (..)
  , Mixed (..)
  , NewtypeKindSig (..)
#if __GLASGOW_HASKELL__ >= 810
  , Standalone (..)
  , VisibleForall (..)
#endif
#if __GLASGOW_HASKELL__ >= 906
  , TypeData (..)
#endif
  ) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Functor.Compose (Compose)
import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Language.Haskell.TH (Exp, Name, Q, runIO)
import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH.Extras (tyConArity')

data KindSig :: Type -> Type -> Type where
  KindSig :: KindSig a b

data Mixed a :: Type -> Type where
  Mixed :: Mixed a b

newtype NewtypeKindSig :: Type -> Type where
  NewtypeKindSig :: Int -> NewtypeKindSig a

#if __GLASGOW_HASKELL__ >= 810
type Standalone :: Type -> Type -> Type
data Standalone a b = Standalone

type VisibleForall :: forall k -> k -> Type
data VisibleForall k a = VisibleForall
#endif

#if __GLASGOW_HASKELL__ >= 906
type data TypeData a b = TypeDataA | TypeDataB
#endif

-- | The type constructors to test, with their expected arities.
arityFixtures :: [(Name, Int)]
arityFixtures =
  [ (''Maybe, 1)
  , (''Either, 2)
  , (''Proxy, 1)
  , (''Const, 2)
  , (''Compose, 3)
  , (''KindSig, 2)
  , (''Mixed, 2)
  , (''NewtypeKindSig, 1)
#if __GLASGOW_HASKELL__ >= 810
  , (''Standalone, 2)
  , (''VisibleForall, 2)
#endif
#if __GLASGOW_HASKELL__ >= 906
  , (''TypeData, 2)
#endif
  ]

-- | Splices in a list of @(name, result, expected)@, where the result is that of
-- 'tyConArity' on the name at compile time, or the message if it called
-- 'error', so that an error is reported as a test failure at run time rather
-- than failing compilation.
arityCases :: Q Exp
arityCases = do
  cases <- mapM arityCase arityFixtures
  lift cases
  where
    arityCase :: (Name, Int) -> Q (String, Either String Int, Int)
    arityCase (name, expected) = do
      -- tyConArity' returns its result lazily, so any error is raised only
      -- when the result is forced, inside 'try'.
      result <- tyConArity' name
      arity <- runIO (try (evaluate (let (ts, ka) = result in length ts + ka)))
      pure (show name, either (\(ErrorCall msg) -> Left msg) Right arity, expected)
