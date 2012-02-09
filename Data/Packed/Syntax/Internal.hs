{-# LANGUAGE ViewPatterns #-}
{- |
Internal parsers. Most users should not need to use this module.
-}
module Data.Packed.Syntax.Internal(
  listExp,
  listPat,
  matListExp,
  matListPat) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified Language.Haskell.Exts as HSE
--import qualified Language.Haskell.Meta.Parse.Careful as MT -- once supported on Hackage
import qualified Language.Haskell.Meta.Parse as MT(parseExp, parsePat)

import Control.Applicative

-- list parsing
wrap s = "[" ++ s ++ "]"

-- | Parser for list expressions
listExp :: String -> Either String [TH.Exp]
listExp s = case MT.parseExp (wrap s) of
  Right (TH.ListE es) -> return es
  Right _ -> fail "unexpected parse"
  Left msg -> fail msg

-- | Parser for list patterns
listPat :: String -> Either String [TH.Pat]
listPat s = case MT.parsePat (wrap s) of
  Right (TH.ListP ps) -> return ps
  Right _ -> fail "unexpected parse"
  Left msg -> fail msg

-- matrix parsing
-- approach to parsing matrices: surround with [] brackets, and repeatedly parse. Will get a parse error with message semiParseError when we encounter an "unexpected" semicolon: we break at this point, and continue parsing
semiParseError = "Parse error: ;"

-- | find the location in the given string, returning everything strictly before; and everything strictly after
-- the character *at* the location is dropped
splitAtLoc :: HSE.SrcLoc -> String -> (String, String)
splitAtLoc loc s = case splitAt (HSE.srcLine loc - 1) (lines s) of
  (linesBefore, line:linesAfter) -> case splitAt (HSE.srcColumn loc - 1) line of
    (lineStart, _:lineEnd) -> (concat linesBefore ++ lineStart, lineEnd ++ concat linesAfter)

breakOnSemis :: (String -> HSE.ParseResult res) -> (String -> Either String res'th) -> String -> Either String [res'th]
breakOnSemis parse parse'th s = case parse wrapped_s of
  HSE.ParseOk{} ->
    case parse'th wrapped_s of
      Right r -> Right [r]
      Left msg -> Left msg
  HSE.ParseFailed loc msg
    | msg == semiParseError ->
        case splitAtLoc loc wrapped_s of
          ('[': h, init -> t) -> (:) <$> parse'th (wrap h) <*> breakOnSemis parse parse'th t
    | otherwise -> Left msg
 where wrapped_s = wrap s


unList (TH.ListE l) = l

-- | Parser for matrix expressions. Returns (outer length, inner length, matrix)
matListExp :: String -> Either String (Int, Int, [[TH.Exp]])
matListExp s = case breakOnSemis HSE.parseExp MT.parseExp s of
  Right rows@(r:_) ->
    let
      rowLen = length (unList r)
      colLen = length rows
    in
     if all (\r' -> length (unList r') == length (unList r)) rows
     then return (rowLen, colLen, map unList rows)
     else fail "Not all rows have the same length"
  Left msg -> fail msg

unPList (TH.ListP l) = l

-- | Parser for matrix patterns. Returns (outer length, inner length, matrix)
matListPat :: String -> Either String (Int, Int, [[TH.Pat]])
matListPat s = case breakOnSemis HSE.parsePat MT.parsePat s of
  Right rows@(r:_) ->
    let
      rowLen = length (unPList r)
      colLen = length rows
    in
     if all (\r' -> length (unPList r') == length (unPList r)) rows
     then return (rowLen, colLen, map unPList rows)
     else fail "Not all rows have the same length"
  Left msg -> fail msg
