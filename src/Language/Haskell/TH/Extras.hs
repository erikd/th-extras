{-# LANGUAGE CPP, ScopedTypeVariables, TemplateHaskell, TypeOperators #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Haskell.TH.Extras where

import Control.Monad
import Data.Generics
import qualified Data.Kind as Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 900
import GHC.Exts (FUN)
#endif
import Language.Haskell.TH
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 914
import Language.Haskell.TH.Syntax
#endif
import Language.Haskell.TH.Datatype.TyVarBndr

intIs64 :: Bool
intIs64 = toInteger (maxBound :: Int) > 2^(32 :: Integer)

replace :: (a -> Maybe a) -> (a -> a)
replace = ap fromMaybe

composeExprs :: [ExpQ] -> ExpQ
composeExprs [] = [| id |]
composeExprs [f] = f
composeExprs (f:fs) = [| $f . $(composeExprs fs) |]

-- | Determines the name of a data constructor. It's an error if the 'Con' binds more than one name (which
-- happens in the case where you use GADT syntax, and give multiple data constructor names separated by commas
-- in a type signature in the where clause).
nameOfCon :: Con -> Name
nameOfCon (NormalC  name _) = name
nameOfCon (RecC     name _) = name
nameOfCon (InfixC _ name _) = name
nameOfCon (ForallC _ _ con) = nameOfCon con
nameOfCon (GadtC [name] _ _)    = name
nameOfCon (GadtC _ _ _)    = error $ "nameOfCon: GadtC: only single constructor names are supported"
nameOfCon (RecGadtC [name] _ _) = name
nameOfCon (RecGadtC _ _ _)    = error $ "nameOfCon: RecGadtC: only single constructor names are supported"

-- |WARNING: discards binders in GADTs and existentially-quantified constructors
argTypesOfCon :: Con -> [Type]
argTypesOfCon (NormalC  _ args) = map snd args
argTypesOfCon (RecC     _ args) = [t | (_,_,t) <- args]
argTypesOfCon (InfixC x _ y)    = map snd [x,y]
argTypesOfCon (ForallC _ _ con) = argTypesOfCon con
argTypesOfCon (GadtC _ args _)    = map snd args
argTypesOfCon (RecGadtC _ args _) = [t | (_,_,t) <- args]

nameOfBinder :: TyVarBndr_ a -> Name
nameOfBinder = tvName

varsBoundInCon :: Con -> [TyVarBndrSpec]
varsBoundInCon (ForallC bndrs _ con) = bndrs ++ varsBoundInCon con
varsBoundInCon _ = []

-- | Determines the term variables bound by a pattern, in left-to-right order.
namesBoundInPat :: Pat -> [Name]
namesBoundInPat pat = case pat of
    VarP name     -> [name]
    AsP name pat' -> name : namesBoundInPat pat'
    -- The expression in a view pattern binds nothing in the enclosing scope.
    ViewP _ pat'  -> namesBoundInPat pat'
    -- Every other constructor binds nothing itself, so just look at all of its
    -- sub-patterns.
    _ -> concat (gmapQ goData pat)
  where
    goData :: Data d => d -> [Name]
    goData d
        | Just p <- cast d = namesBoundInPat p
        -- Patterns cannot bind variables inside expressions or types.
        | Just (_ :: Exp) <- cast d = []
        | Just (_ :: Type) <- cast d = []
        | otherwise = concat (gmapQ goData d)


-- | Determines the names a declaration brings into scope: the declared thing itself, plus any data
-- constructors and record fields, class methods and associated families, or pattern synonym record fields.
-- Class default methods and instance method definitions define names declared elsewhere, so they are not
-- included, but data constructors of associated data instances are.
namesBoundInDec :: Dec -> [Name]
namesBoundInDec dec = case dec of
    FunD name _                                       -> [name]
    ValD pat _ _                                      -> namesBoundInPat pat
    DataD _ name _ _ cons _                           -> name : concatMap namesBoundInCon cons
    NewtypeD _ name _ _ con _                         -> name : namesBoundInCon con
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 906
    TypeDataD name _ _ cons                           -> name : concatMap namesBoundInCon cons
#endif
    TySynD name _ _                                   -> [name]
    ClassD _ name _ _ decs                            -> name : concatMap namesBoundInClassDec decs
    ForeignD (ImportF _ _ _ name _)                   -> [name]
    OpenTypeFamilyD (TypeFamilyHead name _ _ _)       -> [name]
    ClosedTypeFamilyD (TypeFamilyHead name _ _ _) _   -> [name]
    DataFamilyD name _ _                              -> [name]
    DataInstD _ _ _ _ cons _                          -> concatMap namesBoundInCon cons
    NewtypeInstD _ _ _ _ con _                        -> namesBoundInCon con
    InstanceD _ _ _ decs                              -> concatMap namesBoundInInstanceDec decs
    PatSynD name (RecordPatSyn fields) _ _            -> name : fields
    PatSynD name _ _ _                                -> [name]
    _                                                 -> []
  where
    namesBoundInClassDec d = case d of
        SigD name _ -> [name]
        OpenTypeFamilyD {} -> namesBoundInDec d
        DataFamilyD {} -> namesBoundInDec d
        _ -> []
    namesBoundInInstanceDec d = case d of
        DataInstD {} -> namesBoundInDec d
        NewtypeInstD {} -> namesBoundInDec d
        _ -> []

-- | Determines the data constructor names and record field names bound by a constructor.
namesBoundInCon :: Con -> [Name]
namesBoundInCon con = case con of
    NormalC name _          -> [name]
    RecC name fields        -> name : [ f | (f, _, _) <- fields ]
    InfixC _ name _         -> [name]
    ForallC _ _ con'        -> namesBoundInCon con'
    GadtC names _ _         -> names
    RecGadtC names fields _ -> names ++ [ f | (f, _, _) <- fields ]

genericalizeName :: Name -> Name
genericalizeName = mkName . nameBase

-- Genericalize all names defined at the top level, to fix the lunacy introduced in GHC 7.2.
-- Why they should be fresh is beyond me; it really seems absurd because there is no way whatsoever
-- to refer to names known to be bound in [d||] quotes other than to scrounge around inside the
-- generated 'Dec's.
genericalizeDecs :: [Dec] -> [Dec]
genericalizeDecs decs = everywhere (mkT fixName) decs
    where
        -- get all names bound in the decs and make them generic
        -- at every occurrence in decs.
        names = decs >>= namesBoundInDec
        genericalizedNames = [ (n, genericalizeName n) | n <- names]
        fixName = replace (`lookup` genericalizedNames)

headOfType :: Type -> Name
headOfType (ForallT _ _ ty) = headOfType ty
headOfType (VarT name) = name
headOfType (ConT name) = name
headOfType (TupleT n) = tupleTypeName n
headOfType ArrowT = ''(->)
headOfType ListT = ''[]
headOfType (AppT t _) = headOfType t
headOfType (SigT t _) = headOfType t
headOfType (UnboxedTupleT n) = unboxedTupleTypeName n
headOfType (UnboxedSumT n) = unboxedSumTypeName n
headOfType (PromotedTupleT n) = tupleDataName n
headOfType PromotedNilT = '[]
headOfType PromotedConsT = '(:)
headOfType StarT = ''Kind.Type
headOfType ConstraintT = ''Kind.Constraint
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 904
headOfType EqualityT = ''(~)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 900
headOfType MulArrowT = ''FUN
#endif
headOfType (PromotedT name) = name
headOfType (ParensT t) = headOfType t
headOfType (InfixT _ name _) = name
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 808
headOfType (AppKindT t _) = headOfType t
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 810
headOfType (ForallVisT _ t) = headOfType t
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 904
headOfType (PromotedInfixT _ name _) = name
#endif
headOfType ty = error $ "headOfType: Unhandled type: " ++ show ty

-- | Determines whether a type variable occurs free in a type, including in kinds and contexts.
-- Binders in a 'forall' telescope scope over the kinds of later binders and over the body.
occursInType :: Name -> Type -> Bool
occursInType var = goType
  where
    goType :: Type -> Bool
    goType ty = case ty of
        VarT name -> name == var
        ForallT bndrs cxt1 ty' -> goBndrs [(tvName b, goData b) | b <- bndrs] (any goType cxt1 || goType ty')
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 810
        ForallVisT bndrs ty' -> goBndrs [(tvName b, goData b) | b <- bndrs] (goType ty')
#endif
        -- Every other constructor binds nothing, so just look at all of its sub-terms.
        _ -> or (gmapQ goData ty)

    goData :: Data d => d -> Bool
    goData d = case cast d of
        Just ty -> goType ty
        Nothing -> or (gmapQ goData d)

    -- Each binder is given as its name and whether 'var' occurs in its kind.
    goBndrs :: [(Name, Bool)] -> Bool -> Bool
    goBndrs bndrs body = foldr (\(name, inKind) rest -> inKind || (name /= var && rest)) body bndrs

-- | Assuming that we're building an instance of the form C (T v_1 ... v_(n-1)) for some GADT T, this function
-- takes a list of the variables v_1 ... v_(n-1) used in the instance head, as well as the result type of some data
-- constructor, say T x_1 ... x_(n-1) x_n, as well as the type t of some argument to it, and substitutes any of
-- x_i (1 <= i <= n-1) occurring free in t (including in kinds) for the corresponding v_i, taking care to avoid
-- name capture by foralls in t: a forall-bound variable whose name clashes with one of the v_i is renamed.
substVarsWith
  :: [Name] -- Names of variables used in the instance head in argument order
  -> Type -- Result type of constructor
  -> Type -- Type of argument to the constructor
  -> Type -- Type of argument with variables substituted for instance head variables.
substVarsWith topVars resultType argType = subst Map.empty argType
  where
    topVars' = reverse topVars
    AppT resultType' _indexType = resultType

    -- Names that a forall-bound variable must not be called, lest it capture a substituted variable.
    avoid = Set.fromList topVars
    -- Names that a renamed forall-bound variable must also not be called.
    taken = Set.union avoid (Set.fromList (listify (const True :: Name -> Bool) argType))
    fresh n = until (\n' -> not (Set.member n' taken)) prime (prime n)
    prime n = mkName (nameBase n ++ "'")

    -- Brings a telescope of binders into scope. Each binder's kind is in the scope of the earlier binders.
    -- Returns the environment for the body, mapping each bound variable to its (possibly new) name.
    bindAll :: Map Name Name -> [TyVarBndr_ flag] -> (Map Name Name, [TyVarBndr_ flag])
    bindAll env [] = (env, [])
    bindAll env (b:rest) =
      let n = tvName b
          n' = if Set.member n avoid then fresh n else n
          (env', rest') = bindAll (Map.insert n n' env) rest
      in (env', mapTVName (const n') (mapTVKind (subst env) b) : rest')

    -- The environment maps each variable bound by an enclosing forall to its (possibly new) name.
    subst :: Map Name Name -> Type -> Type
    subst bs ty = case ty of
      -- Several of the following cases could all be covered by an "x -> x" case, but
      -- I'd rather know if new cases need to be handled specially in future versions
      -- of Template Haskell.
      ForallT bndrs cxt1 t ->
        let (bs', bndrs') = bindAll bs bndrs
        in ForallT bndrs' (map (subst bs') cxt1) (subst bs' t)
      AppT f x -> AppT (subst bs f) (subst bs x)
      SigT t k -> SigT (subst bs t) (subst bs k)
      VarT v -> case Map.lookup v bs of
        Just v' -> VarT v'
        Nothing -> VarT (findVar v topVars' resultType')
      ConT n -> ConT n
      TupleT k -> TupleT k
      ArrowT -> ArrowT
      ListT -> ListT
      UnboxedTupleT k -> UnboxedTupleT k
      InfixT t1 x t2 -> InfixT (subst bs t1) x (subst bs t2)
      ParensT t -> ParensT (subst bs t)
      UInfixT t1 x t2 -> UInfixT (subst bs t1) x (subst bs t2)
      WildCardT -> WildCardT
      UnboxedSumT k -> UnboxedSumT k
      EqualityT -> EqualityT
      ConstraintT -> ConstraintT
      LitT l -> LitT l
      PromotedConsT -> PromotedConsT
      PromotedNilT -> PromotedNilT
      PromotedT n -> PromotedT n
      PromotedTupleT k -> PromotedTupleT k
      StarT -> StarT
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 904
      PromotedInfixT t1 x t2 -> PromotedInfixT (subst bs t1) x (subst bs t2)
      PromotedUInfixT t1 x t2 -> PromotedUInfixT (subst bs t1) x (subst bs t2)
#endif
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 900
      MulArrowT -> MulArrowT
#endif
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 810
      ForallVisT bndrs t ->
        let (bs', bndrs') = bindAll bs bndrs
        in ForallVisT bndrs' (subst bs' t)
#endif
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 808
      ImplicitParamT s t -> ImplicitParamT s (subst bs t)
      AppKindT f x -> AppKindT (subst bs f) (subst bs x)
#endif

    findVar v (tv:_) (AppT _ (VarT v')) | v == v' = tv
    -- Skip any other parameter, which may be a variable or (in a GADT) a refined type.
    findVar v (_:tvs) (AppT t _) = findVar v tvs t
    findVar v _ _ = error $ "substVarsWith: couldn't look up variable substitution for " ++ show v
      ++ " with topVars: " ++ show topVars ++ " resultType: " ++ show resultType ++ " argType: " ++ show argType

-- | Determine the arity of a kind.
-- Starting in template-haskell 2.8.0.0, 'Kind' and 'Type' became synonymous.
kindArity :: Kind -> Int
kindArity k = case k of
  ForallT _ _ t -> kindArity t
  AppT (AppT ArrowT _) t -> 1 + kindArity t
  SigT t _ -> kindArity t
  ParensT t -> kindArity t
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 810
  -- Each visibly quantified variable (forall k ->) is an argument.
  ForallVisT bndrs t -> length bndrs + kindArity t
#endif
  _ -> 0

-- | Given the name of a type constructor, determine its full arity
tyConArity :: Name -> Q Int
tyConArity n = do
  (ts, ka) <- tyConArity' n
  return (length ts + ka)

-- | Given the name of a type constructor, determine a list of type variables bound as parameters by
-- its declaration, and the arity of the kind of type being defined (i.e. how many more arguments would
-- need to be supplied in addition to the bound parameters in order to obtain an ordinary type of kind *).
-- If the supplied 'Name' is anything other than a data or newtype, produces an error.
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 908
tyConArity' :: Name -> Q ([TyVarBndrUnit], Int)
#else
tyConArity' :: Name -> Q ([TyVarBndr BndrVis], Int)
#endif
tyConArity' n = do
  r <- reify n
  return $ case r of
    TyConI (DataD _ _ ts mk _ _) -> (ts, fromMaybe 0 (fmap kindArity mk))
    TyConI (NewtypeD _ _ ts mk _ _) -> (ts, fromMaybe 0 (fmap kindArity mk))
    _ -> error $ "tyConArity': Supplied name reified to something other than a data declaration: " ++ show n

-- | Determine the constructors bound by a data or newtype declaration. Errors out if supplied with another
-- sort of declaration.
decCons :: Dec -> [Con]
decCons d = case d of
  DataD _ _ _ _ cs _ -> cs
  NewtypeD _ _ _ _ c _ -> [c]
  _ -> error "decCons: Declaration found was not a data or newtype declaration."

-- | Determine the arity of a data constructor.
conArity :: Con -> Int
conArity c = case c of
  NormalC _ ts -> length ts
  RecC _ ts -> length ts
  InfixC _ _ _ -> 2
  ForallC _ _ c' -> conArity c'
  GadtC _ ts _ -> length ts
  RecGadtC _ ts _ -> length ts
