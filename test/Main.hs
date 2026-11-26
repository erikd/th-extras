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
prop_substVarsWith = withTests 1000 . property $ do
  sc <- forAll genSubstCase
  let result = substVarsWith (scTopVars sc) (scResultType sc) (scArgType sc)
      rename v = Map.findWithDefault v v (scExpected sc)
  annotateShow result
  freeVars result === Set.map rename (freeVars (scArgType sc))
