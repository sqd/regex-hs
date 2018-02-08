# regex-hs

`data Regex`: a preprocessed Regex.

`RegexHS.Parse.parse :: String -> Either Parsec.ParseError Regex`: parse a string to a `Regex`.

`RegexHS.Regex.match :: Capture -> Regex -> String -> [Match]`: match a string with a `Regex`, with `Capture` as backreference captures.

`RegexHS.Repr.stringify :: Regex -> String`: reconstruct a `Regex` to its string representation. (`show` returns `Regex`'s raw structure).

See Haddock for details.
