module PureScript.CoreFn where

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode (class DecodeJson, (.?), decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (fromJust)
import Data.StrMap (StrMap)
import Data.String as String
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Prelude

data Module = Module
  Version
  (Array Export)
  (Array Import)
  (Array Foreign)
  (Array (StrMap Decl))

newtype Version = Version String

newtype Export = Export String

newtype Import = Import String

newtype Foreign = Foreign String

newtype Decl = Decl Expr

data Expr
  = Var Name
  | Abs String Expr
  | App Expr Expr
  | Let (Array (StrMap Expr)) Expr
  | Case (Array Expr) (Array Alt)
  | Accessor String Expr
  | LiteralE (Literal Expr)

data Name
  = Local String
  | Global (Array String) String

data Binder
  = NullBinder
  | VarBinder String
  | NamedBinder String Binder
  | LiteralB (Literal Binder)
  | ConstructorBinder Name Name (Array Binder)

data GuardedExpr = GuardedExpr Expr Expr

data Literal a
  = ArrayLiteral (Array a)
  | BooleanLiteral Boolean
  | CharLiteral Char
  | IntLiteral Int
  | NumberLiteral Number
  | ObjectLiteral (StrMap a)
  | StringLiteral String

data Alt = Alt (Array Binder) (Array GuardedExpr)

instance decodeJsonModule :: DecodeJson Module where
  decodeJson = decodeJson >=> \object -> do
    version <- object .? "builtWith"
    exports <- object .? "exports"
    imports <- object .? "imports"
    foreigns <- object .? "foreign"
    decls <- object .? "decls"
    pure $ Module version exports imports foreigns decls

instance decodeJsonVersion :: DecodeJson Version where
  decodeJson = map Version <<< decodeJson

instance decodeJsonImport :: DecodeJson Import where
  decodeJson = map Import <<< decodeJson

instance decodeJsonExport :: DecodeJson Export where
  decodeJson = map Export <<< decodeJson

instance decodeJsonForeign :: DecodeJson Foreign where
  decodeJson = map Foreign <<< decodeJson

instance decodeJsonDecl :: DecodeJson Decl where
  decodeJson = map Decl <<< decodeJson

instance decodeJsonExpr :: DecodeJson Expr where
  decodeJson = decodeJson >=> case _ of
    [var, name] | var == Json.fromString "Var" ->
      Var <$> decodeJson name
    [abs, param, body] | abs == Json.fromString "Abs" ->
      Abs <$> decodeJson param <*> decodeJson body
    [app, func, arg] | app == Json.fromString "App" ->
      App <$> decodeJson func <*> decodeJson arg
    [let_, bindings, body] | let_ == Json.fromString "Let" ->
      Let <$> decodeJson bindings <*> decodeJson body
    [case_, values, alts] | case_ == Json.fromString "Case" ->
      Case <$> decodeJson values <*> decodeJson alts
    [accessor, prop, obj] | accessor == Json.fromString "Accessor" ->
      Accessor <$> decodeJson prop <*> decodeJson obj
    [literal, inner] | literal == Json.fromString "Literal" ->
      LiteralE <$> decodeJson inner
    other -> throwError $ "Could not decode CoreFn expression: " <> Json.stringify (encodeJson other)

instance decodeJsonName :: DecodeJson Name where
  decodeJson = map make <<< decodeJson
    where
    make :: String -> Name
    make = String.split (String.Pattern ".") >>> case _ of
      [name] -> Local name
      parts -> unsafePartial fromJust $
        Global <$> Array.init parts <*> Array.last parts

instance decodeJsonAlt :: DecodeJson Alt where
  decodeJson = decodeJson >=> case _ of
    [binders, values] ->
      Alt <$> decodeJson binders
          <*> (     (yesGuards values)
                <|> (noGuards <$> decodeJson values)
              )
    _ -> throwError "Could not decode CoreFn expression"
    where
    noGuards :: Expr -> Array GuardedExpr
    noGuards e = [GuardedExpr (LiteralE (BooleanLiteral true)) e]

    yesGuards :: Json -> Either String (Array GuardedExpr)
    yesGuards = decodeJson >=> traverse case _ of
      [guard, expr] -> GuardedExpr <$> decodeJson guard <*> decodeJson expr
      other -> throwError "Could not decode CoreFn case alternative"

instance decodeJsonBinder :: DecodeJson Binder where
  decodeJson binder
    | binder == Json.fromString "NullBinder" = pure NullBinder
    | otherwise = decodeJson binder >>= case _ of
        [varBinder, name] | varBinder == Json.fromString "VarBinder" ->
          VarBinder <$> decodeJson name
        [namedBinder, name, inner] | namedBinder == Json.fromString "NamedBinder" ->
          NamedBinder <$> decodeJson name <*> decodeJson inner
        [literalBinder, inner] | literalBinder == Json.fromString "LiteralBinder" ->
          LiteralB <$> decodeJson inner
        [ctorBinder, type_, ctor, values] | ctorBinder == Json.fromString "ConstructorBinder" ->
          ConstructorBinder <$> decodeJson type_ <*> decodeJson ctor <*> decodeJson values
        other -> throwError $ "Could not decode CoreFn binder: " <> Json.stringify (encodeJson other)

instance decodeJsonLiteral :: (DecodeJson a) => DecodeJson (Literal a) where
  decodeJson = decodeJson >=> case _ of
    [arrayLiteral, values] | arrayLiteral == Json.fromString "ArrayLiteral" ->
      ArrayLiteral <$> decodeJson values
    [booleanLiteral, value] | booleanLiteral == Json.fromString "BooleanLiteral" ->
      BooleanLiteral <$> decodeJson value
    [charLiteral, value] | charLiteral == Json.fromString "CharLiteral" ->
      CharLiteral <$> decodeJson value
    [intLiteral, value] | intLiteral == Json.fromString "IntLiteral" ->
      IntLiteral <$> decodeJson value
    [numberLiteral, value] | numberLiteral == Json.fromString "NumberLiteral" ->
      NumberLiteral <$> decodeJson value
    [objectLiteral, value] | objectLiteral == Json.fromString "ObjectLiteral" ->
      ObjectLiteral <$> decodeJson value
    other -> throwError $ "Could not decode CoreFn literal: " <> Json.stringify (encodeJson other)
