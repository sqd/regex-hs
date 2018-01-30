module Parse
    ( parser
    ) where

import Regex

import Text.Parsec
import Text.Parsec.Char

-- | The parser type that takes a string and spits out a Regex AST.
-- The state Int is the next capture index.
type Parser = Parsec String Int Regex

-- | Parse @\d@. Not named "digit" to avoid conflict with Parsec.
num :: Parser
num = string "\\d" >> return Digit

-- | Parse @\w@.
word :: Parser
word = string "\\w" >> return WordCharacter

-- | Parse @\s@.
whitespace :: Parser
whitespace = string "\\s" >> return Whitespace

-- | Parse an escaped character.
escapedCharacter :: Parser
escapedCharacter = char '\\' >> anyChar >>= (return . Character)

-- | Parse a capture group.
capture :: Parser
capture = do
    exp <- between (char '(') (char ')') exprSeq
    i <- getState
    putState (i+1)
    return $ Capture i exp

-- | Parse a non-capture group.
nonCaptureGroup :: Parser
nonCaptureGroup = between (string "(?:") (char ')') exprSeq

-- | Parse a lookahead capture group.
lookAheadGroup :: Parser
lookAheadGroup = LookAhead <$> between (string "(?=") (char ')') exprSeq

-- | Parse a comment bracket.
comment :: Parser
comment = do
    string "(?#"
    manyTill anyChar $ char ')'
    return Dummy

-- | Parse a square bracket like @[abc1-9]@.
-- __Not fully working yet.__
set :: Parser
set = do
    -- escape not fully working, for example \W is now parsed as just W
    let parseChar = try (char '\\' >> anyChar) <|> noneOf "-]"
        range = do
            lb <- parseChar
            char '-'
            ub <- parseChar
            return [lb .. ub]
        singleChar = do
            c <- parseChar
            return [c]

    char '['
    l <- many (try range <|> try singleChar)
    char ']'
    return $ concat l
    undefined

-- | Parse a non-composite (but maybe nested) regex.
singletonExpr :: Parser
singletonExpr = do
    s <- choice $ map try
        [ num
        , word
        , whitespace
        -- escaped tokens go above this line --
        , escapedCharacter
        -------

        -- bracket squad
        , nonCaptureGroup
        , lookAheadGroup
        , comment
        , capture

        -- no parser should be added to below this line --
        -- `notFollowedBy` allows `singletonExpr` to fail on starts of elements that previous parsers failed to parse, or characters that can't start a singleton.
        , do notFollowedBy $ oneOf "()*+?{}[]|"
             Character <$> anyChar
        ]

    -- Try repetition.
    choice $ map try
        [ string "*?" >> (return $ NoneOrMoreLazy s)
        , char '*' >> (return $ NoneOrMore s)
        , string "+?" >> (return $ onceOrMoreLazy s)
        , char '+' >> (return $ onceOrMore s)
        , string "??" >> (return $ NoneOrOnceLazy s)
        , char '?' >> (return $ NoneOrOnce s)
        , do char '{'
             d <- many1 digit
             char '}'
             return $ times s $ read d
        , do char '{'
             lb <- many1 digit
             char ','
             ub <- many1 digit
             char '}'
             return $ range s (read lb) (read ub)
        -- we tried
        , return s ]

-- | Parse a sequence of singleton expressions, or alternatives (@a|b@).
exprSeq :: Parser
exprSeq = do
    -- Try parsing alternatives, with each option being a sequence of consecutive singleton expressions. If the expression is not of alternative form, singletons would just be a list with only one element.
    singletons <- sepBy (many singletonExpr) (char '|')
    -- If a option-sequence is empty (could happen!), just return `Dummy`.; otherwise, concantate them together.
    let chained = (\l -> if null l then Dummy else foldl1 Cons l) <$> singletons
    -- If there is no sequence, return `Dummy`; otherwise, if there is just one option-sequence, this is not an alternative construct, so we just extract the sequence; else, this is an alternaive structure, so we chain the sequences with `Alternative`.
    return $ case chained of
        [] -> Dummy
        [x] -> x
        _ -> foldl1 Alternative chained

-- | Parse a string to a regex.
parser :: Parser
parser = do
    rst <- exprSeq
    -- Failure if there is any unparsed leftover.
    notFollowedBy anyChar
    return rst
