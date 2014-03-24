--  argError = addError' ["When attempting to unify the argument types of `"
--                       , render type1, "' and `", render type2, "'"]
--  returnError = addError' ["When attempting to unify the return types of `"
--                          , render type1, "' and `", render type2, "'"]
--  catchError' = catchError
--  incompatErr =
--      throwErrorC [ "Incompatible types: `", render type1, "' and `"
--                  , render type2, "'"]
--  ambiguousErr t1 t2 = throwErrorC [
--      "Ambiguous application of `", render type1, "' to `", render type2, "'. "
--    , "Multiple unification choices are equally valid: could be `", render t1
--    , "', or `", render t2, "'"
--    ]
--  tryingErr valids = addError' ["After finding potential matches ", r valids]
--  r valids = T.intercalate ", " $ map (fst ~> render) valids
--  noMatchErr = throwErrorC ["No types exist in multifunction's set which match ",
--                            render type2]
--  noArgMatchErr argT set =
--    throwErrorC ["No types in the set `", render (M.keys set), "' match the "
--                , "provided argument type `", render argT, "'"]


--instance Typable Expr where
--  typeOf expr = go `catchError` err where
--    condError = addError "Condition should be a Bool"
--    branchError = addError "Parallel branches must resolve to the same type"
--    scopeError name = throwErrorC ["'", name, "' is already defined in scope"]
--    go = case expr of
--      Number _ -> return numT
--      String _ -> return strT
--      TypeDef name typ -> addTypeAlias name typ >> return unitT
--      Block blk -> pushNameSpace "%b" *> typeOf blk <* popNameSpace
--      Var name -> lookupAndInstantiate name
--      Constructor name -> lookupAndInstantiate name
--      Array arr@(ArrayLiteral _) -> typeOfArrayLiteral typeOf arr
--      Tuple vals -> tTuple <$> mapM typeOf vals
--      Mutable expr -> TMod TMut_ <$> typeOf expr
--      Lambda arg expr -> do
--        argT <- pushNameSpace "%l" *> litTypeOf arg
--        returnT <- typeOf expr
--        (refine (argT ==> returnT) >>= gen) <* popNameSpace
--      Dot a b -> typeOf (Apply b a)
--      Apply a b -> typeOfApply typeOf a b
--      If' cond res -> do
--        condT <- typeOf cond
--        unifyAdd condT boolT `catchError` condError
--        maybeT <$> typeOf res
--      If cond tBranch fBranch -> do
--        -- Type the condition, make sure it's a Bool
--        condType <- typeOf cond
--        condType `unifyAdd` boolT `catchError` condError
--        -- Type the true and false branches, make sure they're the same type
--        trueType <- typeOf tBranch
--        falseType <- typeOf fBranch
--        trueType `unifyAdd` falseType `catchError` branchError
--        -- Return the true branch's refined type (same as false's)
--        refine trueType
--      Define name expr ->
--        lookup1 name >>= \case
--          -- If it's not defined in this scope, we proceed forward.
--          Nothing -> do
--            -- Initialize this name as an unknown type variable, for in case
--            -- this is a recursive definition
--            var <- unusedTypeVar
--            record name var
--            result <- pushNameSpace name *> typeOf expr <* popNameSpace
--            var' <- refine var
--            -- Make sure the types unify
--            unify (var', result) `catchError` definitionError name
--            refine result >>= record name
--          Just typ -> scopeError name
--      Extend name expr@(Lambda arg body) ->
--        lookup name >>= \case
--          -- If it's not defined in this scope, we proceed forward.
--          Nothing -> notInScopeError name
--          Just f@(TFunction from to) -> extend name f expr
--          Just mf@(TMultiFunc tset) -> extend name mf expr
--          Just _ -> notAFunctionError
--      Extend _ _ -> notAFunctionError
--      Assign expr expr' -> do
--        exprT <- typeOf expr
--        -- check mutability of exprT here?
--        exprT' <- typeOf expr'
--        unify (exprT, exprT')
--        refine exprT
--      While cond block -> do
--        pushNameSpace "%while"
--        cType <- typeOf cond
--        unify (cType, boolT) `catchError` condError
--        maybeT <$> typeOf block <* popNameSpace
--      Return expr -> typeOf expr
--      _ -> error $ T.unpack $ "we can't handle expression `" <> render expr <> "'"
--    err = addError' ["When typing the expression `", render expr, "'"]
--    definitionError name =
--      addError' ["When attempting to unify the perceived type of '", name, "' "
--                , "(as determined from its declared arguments) with how it is "
--                , "used recursively in its definition."]
--    itemError name =
--      addError' ["When unifying declared type of iterating variable '", name
--                , "' with what its container contains"]
--    notAFunctionError =
--      throwErrorC [ "Only functions' definitions can be extended, and "
--                  , "only with other functions."]
--    notInScopeError name =
--      throwErrorC [ "Attempted to extend the definition of '", name, "', ",
--                    "but that name is not in scope."]
--    extend name origType expr = do
--      -- the expr must be a Lambda with declared argument type.
--      argT <- case expr of
--        Lambda arg body -> litTypeOf arg
--        _ -> notAFunctionError
--      -- We don't know what the return type is; initialize it as unknown.
--      retT <- unusedTypeVar
--      -- Add in this new definition to the current
--      record name (origType <> (argT ==> retT))
--      result <- pushNameSpace name *> typeOf expr <* popNameSpace
--      -- TODO: Make sure the types unify.
--      typ <- refine result
--      record name (origType <> typ)
--      case typ of
--        TFunction _ _ -> record name (origType <> typ)
--        TMultiFunc  _ -> record name (origType <> typ)
--        _ -> notAFunctionError

--instance Typable (Expr, Block) where
--  typeOf (arg, block) = do
--    argType <- litTypeOf arg
--    TFunction argType <$> typeOf block

-- | takes a polytype and replaces any type variables in the type with unused
-- variables.
insta_ :: Type -> Typing Type
insta_ typ = fst <$> runStateT (inst typ) mempty where
  inst :: Type -> StateT (M.Map Name Type) Typing Type
  inst typ' = case typ' of
    TRigidVar _ -> return typ'
    TConst _ -> return typ'
    TVar name -> do
      M.lookup name <$> get >>= \case
        -- if we haven't yet seen this variable, create a new one
        Nothing -> do typ'' <- lift unusedTypeVar
                      modify $ M.insert name typ''
                      return typ''
        -- otherwise, return what we already created
        Just typ'' -> return typ''
    TApply a b -> TApply <$$ inst a <*> inst b
    TFunction a b -> TFunction <$$ inst a <*> inst b
    TTuple ts -> TTuple <$> mapM inst ts
    TMut typ'' -> TMut <$> inst typ''
    TMultiFunc tset -> do
      list' <- forM (M.toList tset) $ \(f, t) -> (,) <$$ inst f <*> inst t
      return $ TMultiFunc (M.fromList list')

-- | the opposite of insta_; it "polymorphizes" the rigid type variables
-- so that they can be polymorphic in future uses.
gen :: Type -> Typing Type
gen typ = fst <$> runStateT (go typ) (mempty, "a") where
  go :: Type -> StateT (M.Map Name Type, Name) Typing Type
  go typ' = case typ' of
    TRigidVar name -> return $ TVar name
    TVar _         -> return typ'
    TConst _       -> return typ'
    TTuple ts      -> TTuple <$> mapM go ts
    TApply a b     -> TApply <$$ go a <*> go b
    TFunction a b  -> TFunction <$$ go a <*> go b

--typeItIO :: String -> IO ()
--typeItIO input = case grab input of
--  Left err -> putStrLn $ "Parse error:\n" <> show err
--  Right block -> case runTyping block of
--    (Left err, _) -> error $ T.unpack $ render err
--    (Right block, state) -> putStrLn $ T.unpack $ render block <> "\n" <> render state
