{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (forM_)
import Fixtures (arityCases)
import Gen
import Hedgehog
import Hedgehog.Main (defaultMain)
import Language.Haskell.TH (mkName, nameBase)
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
        , ("prop_namesBoundInDec", prop_namesBoundInDec)
        , ("prop_genericalizeDecs", prop_genericalizeDecs)
        , ("prop_headOfType_builtin", prop_headOfType_builtin)
        , ("prop_substVarsWith_existential", prop_substVarsWith_existential)
        , ("prop_tyConArity", prop_tyConArity)
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

-- | As 'prop_substVarsWith', but the argument type also mentions existential
-- variables and the index variable, which are not parameters of the result
-- type and so must be left unchanged.
prop_substVarsWith_existential :: Property
prop_substVarsWith_existential = withTests 1000 . property $ checkSubstCase =<< forAll genExistentialSubstCase

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

-- | 'namesBoundInDec' finds every name a declaration brings into scope,
-- including data constructors, record fields, class methods, associated
-- families and pattern synonym fields.
prop_namesBoundInDec :: Property
prop_namesBoundInDec = withTests 1000 . property $ do
  dec <- forAll genDec
  namesBoundInDec dec === decNames dec

-- | 'genericalizeDecs' replaces every occurrence of every name bound at the top
-- level of the declarations with a generic name, and leaves all other names
-- (including fresh ones bound locally) alone.
prop_genericalizeDecs :: Property
prop_genericalizeDecs = withTests 1000 . property $ do
  decs <- forAll genQuotedDecs
  let bound = Set.fromList (concatMap decNames decs)
      expected n = if Set.member n bound then mkName (nameBase n) else n
  allNames (genericalizeDecs decs) === map expected (allNames decs)

-- | 'headOfType' finds the type constructor at the head of a type whose head is
-- a built-in type constructor with dedicated Template Haskell syntax, such as
-- unboxed sums, promoted lists and tuples, 'Type', 'Constraint', equality and
-- the multiplicity-polymorphic arrow.
prop_headOfType_builtin :: Property
prop_headOfType_builtin = withTests 1000 . property $ do
  (ty, name) <- forAll genBuiltinHeadedType
  headOfType ty === name

-- | 'tyConArity' gives the number of type arguments a data type or newtype
-- takes, however its parameters are declared: binders, kind signatures,
-- standalone kind signatures or @type data@. The arities are computed at
-- compile time by reifying the declarations in "Fixtures".
prop_tyConArity :: Property
prop_tyConArity = withTests 1 . property $
  forM_ ($(arityCases) :: [(String, Either String Int, Int)]) $ \(name, result, expected) ->
    (name, result) === (name, Right expected)
