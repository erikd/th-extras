{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Gen
import Hedgehog
import Hedgehog.Main (defaultMain)
import Language.Haskell.TH.Extras

main :: IO ()
main =
  defaultMain
    [ checkParallel $ Group "Language.Haskell.TH.Extras"
        [ ("prop_occursInType", prop_occursInType)
        , ("prop_namesBoundInPat", prop_namesBoundInPat)
        , ("prop_substVarsWith", prop_substVarsWith)
        , ("prop_headOfType", prop_headOfType)
        , ("prop_kindArity", prop_kindArity)
        , ("prop_substVarsWith_refined", prop_substVarsWith_refined)
        ]
    ]

-- | 'occursInType' agrees with the reference free-variable function for
-- every kind of type.
prop_occursInType :: Property
prop_occursInType = withTests 1000 . property $ do
  ty <- forAll genType
  v <- forAll genVarName
  occursInType v ty === Set.member v (freeVars ty)

-- | 'namesBoundInPat' finds every variable bound by a pattern, in order.
prop_namesBoundInPat :: Property
prop_namesBoundInPat = withTests 1000 . property $ do
  pat <- forAll genPat
  namesBoundInPat pat === patVars pat

-- | 'substVarsWith' renames exactly the free variables of the argument type:
-- no free variable is missed and no substituted variable is captured by a
-- binder in the argument type.
prop_substVarsWith :: Property
prop_substVarsWith = withTests 1000 . property $ checkSubstCase =<< forAll genSubstCase

-- | As 'prop_substVarsWith', but for GADT constructors whose result type refines
-- some of the type constructor's parameters to concrete types.
prop_substVarsWith_refined :: Property
prop_substVarsWith_refined = withTests 1000 . property $ checkSubstCase =<< forAll genRefinedSubstCase

checkSubstCase :: SubstCase -> PropertyT IO ()
checkSubstCase sc = do
  let result = substVarsWith (scTopVars sc) (scResultType sc) (scArgType sc)
      rename v = Map.findWithDefault v v (scExpected sc)
  -- Force the whole result here, so that an 'error' call is reported as a
  -- failure of this property rather than escaping from 'annotateShow'.
  _ <- eval (length (show result))
  annotateShow result
  freeVars result === Set.map rename (freeVars (scArgType sc))

-- | 'headOfType' sees through applications, parentheses, kind signatures,
-- foralls and kind applications to the name at the head of a type.
prop_headOfType :: Property
prop_headOfType = withTests 1000 . property $ do
  (ty, name) <- forAll genHeadedType
  headOfType ty === name

-- | 'kindArity' counts every argument a kind takes, including visible
-- dependent quantification, looking through parentheses, kind signatures and
-- invisible foralls.
prop_kindArity :: Property
prop_kindArity = withTests 1000 . property $ do
  (k, n) <- forAll genKindWithArity
  kindArity k === n
