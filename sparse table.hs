{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.Eq
import Data.Ord
import Data.Int
import Data.Bits
import Data.Char
import Data.List
import Data.IORef
import Data.Maybe
import Data.Ratio
import Data.Monoid
import Data.Either
import Data.String
import Data.Function
--import Data.Array
import Data.Array.ST
import Data.Array.Unboxed

import System.IO
import System.IO.Unsafe
import System.Exit
--import System.Random

import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Fail
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Except

import qualified Data.ByteString.Char8 as C

il2 :: Int -> Int
il2 x = finiteBitSize x - 1 - countLeadingZeros x

data SparseTable = SparseTable { table :: UArray (Int, Int) Int, func :: Int -> Int -> Int }

buildSparse :: Array Int Int -> (Int -> Int -> Int) -> SparseTable
buildSparse e f = SparseTable (runSTUArray $ do
    let n = length e
    let lg = il2 n
    let pws = (listArray (0, lg - 1) $ take lg $ iterate (*2) 1) :: Array Int Int
    sp <- newArray ((0, 0), (lg, n)) 0
    forM_ [0..n - 1] $ \j -> do
        writeArray sp (0, j) (e ! j)
    forM_ [1..lg] $ \i ->
        let p = (pws!(i - 1)) in
        forM_ [0..(n - 2 * p)] $ \j -> do
            l <- readArray sp (i - 1, j)
            r <- readArray sp (i - 1, j + p)
            writeArray sp (i, j) $ f l r
    return sp) f

sparseQuery :: SparseTable -> Int -> Int -> Int
sparseQuery (SparseTable sp f) l r = let lg = il2 (r - l + 1); pws = iterate (*2) 1
    in f (sp!(lg, l)) (sp!(lg, r - (pws!!lg) + 1))