{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
        ]
    ]

-- | 'occursInType' agrees with the reference free-variable function for
-- every kind of type.
prop_occursInType :: Property
prop_occursInType = withTests 1000 . property $ do
  ty <- forAll genType
  v <- forAll genVarName
  occursInType v ty === Set.member v (freeVars ty)
