module RegexHS.Test
    ( parse
    , match
    , stringify
    , try
    ) where

import qualified Regex
import qualified Repr
import qualified Parse
import Text.Parsec (runP)
import qualified Data.Map.Strict as M

parse = runP Parse.parser 1 ""
match exp s =
    let rst = Regex.match (M.singleton 0 "") exp s
    in case rst of
        [] -> Left "Test.match: No match"
        l -> Right $ head l
matchRaw exp s =
    let rst = Regex.match (M.singleton 0 "") exp s
    in case rst of
        [] -> Left "Test.match: No match"
        l -> Right l
stringify = Repr.stringify
try exp s = case parse exp of
    Left err -> Left $ show err
    Right r -> match r s 

tryRaw exp s = case parse exp of
    Left err -> Left $ show err
    Right r -> matchRaw r s 
