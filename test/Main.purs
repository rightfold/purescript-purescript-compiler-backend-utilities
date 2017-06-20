module Test.Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.StrMap (StrMap)
import Debug.Trace (traceAnyA)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Prelude
import PureScript.CoreFn (Module)

main :: ∀ eff. Eff (exception :: EXCEPTION, fs :: FS | eff) Unit
main = do
  json <- readTextFile UTF8 "output/Data.Array/corefn.json"
  let mod = decodeJson <=< jsonParser $ json
  traceAnyA (mod :: Either String (StrMap Module))
