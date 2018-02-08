module RegexHS.Repr
    (stringify) where

import Regex

-- | Add brackets to a string reresentation if necessary.
singletonize :: Regex -> String

singletonize x@Dummy = undefined
singletonize x@(Character _) = stringify x
singletonize x@Digit = stringify x
singletonize x@WordCharacter = stringify x
singletonize x@Whitespace = stringify x
singletonize x@(Ref i) = stringify x
singletonize x = "(" ++ stringify x ++ ")"

-- | Reconstrcut a string regex from an AST.
stringify :: Regex -> String

stringify (Cons a b) = (stringify a) ++ (stringify b)
stringify Dummy = ""
stringify (Character c) = [c]
stringify Digit = "\\d"
stringify WordCharacter = "\\w"
stringify Whitespace = "\\s"
stringify (Capture i exp) = "(" ++ stringify exp ++ ")"
stringify (Ref i) = "\\" ++ show i
stringify (NoneOrOnce exp) = singletonize exp ++ "?"
stringify (NoneOrOnceLazy exp) = singletonize exp ++ "??"
stringify (NoneOrMore exp) = singletonize exp ++ "*"
stringify (NoneOrMoreLazy exp) = singletonize exp ++ "*?"
stringify (UpTo exp n) = singletonize exp ++ "{0," ++ show n ++ "}"
stringify (UpToLazy exp n) = singletonize exp ++ "{0," ++ show n ++ "}?"
stringify (LookAhead exp) = "(?=" ++ stringify exp ++ ")"
stringify (Alternative a b) = stringify a ++ "|" ++ stringify b
