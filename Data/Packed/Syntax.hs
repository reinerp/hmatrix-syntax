{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-name-shadowing #-}

-- |
-- The main import. Modules using these quasiquoters need the following language pragma:
--
-- > {-# LANGUAGE QuasiQuotes, ViewPatterns #-}
module Data.Packed.Syntax(vec, mat) where

import Data.Packed.Syntax.Internal

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote as TH
import Language.Haskell.TH.Syntax as TH

import Data.Packed.Vector(
  Vector,
  dim,
  )
import Data.Packed.Matrix(
  Matrix,
  rows,
  cols,
  )
import Data.Packed.ST(
  runSTVector,
  newUndefinedVector,
  unsafeWriteVector,
  runSTMatrix,
  newUndefinedMatrix,
  unsafeWriteMatrix,
 )
import Data.Packed.Development(
  MatrixOrder(..),
  at',
  atM',
  )

-- | Quasiquoter for vectors. For example, use as an expression:
--
-- > buildVec x = [vec| x, sin x |]
--
-- or use as a pattern:
--
-- > swap [vec| x, y |] = [vec| y, x |]
vec :: QuasiQuoter
vec = qq vecExp vecPat

-- | Quasiquoter for matrices. For example, use as an expression:
--
-- > buildMat x y = [mat| x,     y;
-- >                      x + y, sin y |]
--
-- or use as a pattern:
--
-- > adjugateMat2 [mat| a, b; c, d |] = [mat| d, -b; -c, a |]
--
-- If row sizes don't match, this will be caught at compile time.
mat :: QuasiQuoter
mat = qq matExp matPat

qq exp pat = QuasiQuoter exp pat (const $ fail "Type quasiquotes not supported") (const $ fail "Declaration quasiquotes not supported")


-- TODO: remove the intermediate lists in the following

-- approach to parsing vectors: surround with [] brackets and parse as a list

vecExp s = case listExp s of
  Right es -> buildVectorST es
  Left msg -> fail msg

buildVectorST es =
  [| runSTVector (do
                     v <- newUndefinedVector $( lift (length es) )
                     $( let buildWrites _i [] = [| return () |]
                            buildWrites i (exp:exps) = [| unsafeWriteVector v i $(return exp) >> $(buildWrites (i+1) exps) |]
                        in buildWrites 0 es)
                     return v) |]

buildToList n =
  [| \vec -> if dim vec /= n
             then Nothing
             else Just $(let
               buildList i | i == n    = [| [] |]
                           | otherwise = [| at' vec i : $(buildList (i+1)) |]
               in buildList 0) |]

vecPat :: String -> Q TH.Pat
vecPat s = case listPat s of
  Right ps ->
    let l = ListP ps in viewP (buildToList (length ps)) (conP 'Just [return l])
  Left msg -> fail msg

matExp s = case matListExp s of
  Right (_, _, rows) -> buildMatST rows
  Left msg -> fail msg

buildMatST :: [[TH.Exp]] -> Q TH.Exp
buildMatST es =
  let r = length es
      c = length (head es)
  in
  [| runSTMatrix
       (do
          m <- newUndefinedMatrix RowMajor r c
          $( let writes = [ [| unsafeWriteMatrix m ir ic $(return $ es !! ir !! ic) |] | ir <- [0..r-1], ic <- [0..c-1] ]
             in foldr (\h t -> [| $h >> $t |]) [| return () |] writes)
          return m
       ) |]

matPat s = case matListPat s of
  Right (rowLen, colLen, rows) ->
    viewP (buildToLists colLen rowLen)
          (conP 'Just [return $ ListP $ map ListP rows])
  Left msg -> fail msg

buildToLists r c =
  [| \m -> if (rows m, cols m) /= (r, c) then Nothing
           else Just
                $( TH.listE [ TH.listE [ [| atM' m ir ic |] | ic <- [0..c-1] ] | ir <- [0..r-1] ] )
   |]
