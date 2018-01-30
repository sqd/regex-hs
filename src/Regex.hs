module Regex
    ( Regex (..)
    , times
    , range
    , onceOrMore
    , onceOrMoreLazy
    , match
    ) where

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List

import Debug.Trace

append :: a -> [a] -> [a]
append a l = l ++ [a]

prepend :: a -> [a] -> [a]
prepend = (:)

data Regex =
    Cons Regex Regex -- ^ @Cons a b@ matches @a@ then matches @b@.
    | Dummy -- ^ Matches an empty string.
    | Character Char -- ^ Matches precisely the character.
    | Digit -- ^ @\d@
    | WordCharacter -- ^ @\w@
    | Whitespace -- ^ @\s@
    | Capture Int Regex -- ^ @Capture i expr@ attempts to match @expr@, and stores the result as the i-th capture group.
    | Ref Int -- ^ @Ref i@ backreferences the i-th capture group. If it hasn't been captured yet, the behavior is not defined.
    | NoneOrOnce Regex -- ^ @expr?@
    | NoneOrOnceLazy Regex -- ^ @expr??@
    | NoneOrMore Regex -- ^ @expr*@
    | NoneOrMoreLazy Regex -- ^ @expr*?@
    | UpTo Regex Int -- ^ @expr{0,n}@
    | UpToLazy Regex Int -- ^ @expr{0,n}?@
    | LookAhead Regex -- ^ (?=expr)
    | Alternative Regex Regex -- ^ @expr1|expr2@
    | Set [Regex] -- ^ Corresponds to [expr1..expr2]. Every expressions in the list should __DETERMINISTICALLY__ match __ONE and ONLY ONE__ character. E.g. @a?@ (not deterministic) and @ab@ (not one) are not legal list elements.
    deriving Show

type Match = M.Map Int String
type Capture = Match

-- | Combine two matches, and the whole matches are concantated together. In @combineMatch a b@, if both @a@ and @b@ both have a capture group numbered the same, the one from @b@ is used.
combineMatch :: Match -> Match -> Match
combineMatch a b = M.insert 0 (a ! 0 ++ b ! 0) $ combineCapture a b

-- | Combine two captures. In @combineCapture a b@, if both @a@ and @b@ both have a capture group numbered the same, the one from @b@ is used.
combineCapture :: Capture -> Capture -> Capture
combineCapture = flip M.union

-- | Extract the full match (capture group number @0@) from a match.
fullMatch :: Match -> String
fullMatch = (! 0)

-- | Set the full match (capture group number @0@) for a match.
setMatch :: Match -> String -> Match
setMatch m s = M.insert 0 s m

-- | Strip the full match (capture group number @0@) of a match from the beginning of a string. So the result is the remaining part yet to be matched.
stripResult :: Match -> String -> String
stripResult m s = fromJust $ stripPrefix (fullMatch m) s

-- | Match an empty string.
skip :: Match
skip = M.fromList [(0, "")]

-- | A match that indicates illegal match. (a dead end in backtracking).
-- Not the same as @skip@!
noMatch :: [Match]
noMatch = []

-- | Match a regex with a string.
match :: Capture -- ^ The captures to use for back-referencing.
    -> Regex -- ^ The regex to match with.
    -> String -- ^ The string to match.
    -> [Match] -- ^ The matches, with an element in the front more "desirable" than one in the back, in terms of laziness. The matches in this list do __NOT__ contain the original captures.

match captures Dummy _ = [setMatch captures ""]

match captures (Cons a b) s = do
    rstA <- match captures a s
    let remain = stripResult rstA s
        -- b should have access to both previous captures and a's captures.
        combinedCapture = combineCapture captures rstA
    rstB <- match combinedCapture b remain
    --  not `combineMatch combinedCapture rstB` because `match` does not carry previous captures.
    return $ combineMatch rstA rstB

match captures (Capture i exp) s = do
    rst <- match captures exp s
    let remain = stripResult rst s
    return $ M.insert i (fullMatch rst) rst

match captures (Ref i) s =
    case stripPrefix (captures ! i) s of
        Just rst -> [setMatch captures (captures ! i)]
        Nothing -> noMatch
        
match _ (Character _) "" = noMatch
match captures (Character c) (x:xs) =
    if c == x
    then let s = [c] in [M.singleton 0 s]
    else noMatch

match captures (NoneOrOnce exp) s = match captures exp s ++ [skip]

match captures (NoneOrOnceLazy exp) s = skip:(match captures exp s)

match captures (NoneOrMore exp) s = append skip $ do
    this <- match captures exp s
    next <- match captures (NoneOrMore exp) $ stripResult this s
    return $ combineMatch this next

match captures (NoneOrMoreLazy exp) s = prepend skip $ do
    this <- match captures exp s
    next <- match captures (NoneOrMoreLazy exp) $ stripResult this s
    return $ combineMatch this next

match captures (UpTo exp 0) _ = [skip]
match captures (UpTo exp n) s = append skip $ do
    this <- match captures exp s 
    next <- match captures (UpTo exp (n-1)) $ stripResult this s
    return $ combineMatch this next

match captures (UpToLazy exp 0) _ = [skip]
match captures (UpToLazy exp n) s = prepend skip $ do
    this <- match captures exp s
    next <- match captures (UpTo exp (n-1)) $ stripResult this s
    return $ combineMatch this next

match captures (LookAhead exp) s =
    case match captures exp s of
        [] -> noMatch
        l -> [skip]

match captures (Alternative a b) s =
    let rstA = match captures a s
        rstB = match captures b s
    in rstA ++ rstB

-- | Expand expressions like @expr{n}@ to @expr..expr..expr@ (n @expr@s).
times :: Regex -> Int -> Regex
times _ 0 = Dummy
times exp n = Cons exp $ times exp (n-1)

-- | Expand expressions like @expr{n,m}@ to @expr..expr..exprexpr{x}@ (n @expr@s followed by @expr{x}@, with @x=m-n@).
range :: Regex -> Int -> Int -> Regex
range exp lb ub = Cons (times exp lb) $ UpTo exp (ub-lb)

-- | Expand @expr+@ to @exprexpr*?@.
onceOrMore :: Regex -> Regex
onceOrMore exp = Cons exp $ NoneOrMore exp

-- | Expand @expr+?@ to @exprexpr*?@.
onceOrMoreLazy :: Regex -> Regex
onceOrMoreLazy exp = Cons exp $ NoneOrMoreLazy exp
