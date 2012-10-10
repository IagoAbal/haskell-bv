{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# LANGUAGE BangPatterns #-}

-- |
-- Module    : Data.BitVector
-- Copyright : (c) Iago Abal, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>
--

module Data.BitVector 
  ( BitVector
  , BV
  , width
  , value
  , bitvec
  , ones, zeros
  , (==.), (/=.)
  , maxV
  , (@.), (@@)
  , (#)
  , zeroExtend, signExtend
  , foldl, foldr
  , reverse  
  , least, most
  , Bits(..)
  , (<<), (>>), (<<<), (>>>)
  , replicate
  ) where

import Control.Exception ( assert )

import Data.Bits
import Data.Ord

import Prelude hiding ( (>>), foldl, foldr, replicate, reverse )


-- | Big-endian "pseudo width-polymorphic" bit-vectors
--
-- Most operations treat a bit-vector of type [k] as of type
-- forall n. (n >= k) => [n].
--
data BitVector
    = BV {
      width :: !Int
    , value :: !Integer
    }

type BV = BitVector

instance Eq BitVector where
  (BV _ a) == (BV _ b) = a == b

instance Ord BitVector where
  compare = comparing value

instance Show BitVector where
  show (BV n a) = "[" ++ show n ++ "]" ++ show a

integerWidth :: Integer -> Int
integerWidth !n = go 1 1
  where go !k !k_max | k_max >= n = k
                     | otherwise  = go (k+1) (2*k_max+1)
{-# INLINE integerWidth #-}

instance Num BitVector where
  (BV n1 a) + (BV n2 b) = BV n $ (a + b) `mod` 2^n
    where n = max n1 n2
  (BV n1 a) * (BV n2 b) = BV n $ (a * b) `mod` 2^n
    where n = max n1 n2
  negate (BV n a) = BV n $ 2^n - a
  abs = id
  signum (BV _ 0) = 0
  signum (BV n a) | testBit a (n-1) = -1
                  | otherwise       = 1
  fromInteger i = BV (integerWidth i) i

-- instance Real BitVector where
--   toRational = toRational . value

-- instance Enum BitVector where
--   toEnum = fromIntegral
--   fromEnum (BV _ a) = assert (a < max_int) $ fromIntegral a
--     where max_int = toInteger (maxBound::Int)

-- instance Integral BitVector where
--   toInteger = value

-- | Fixed-width equals
--
(==.) :: BV -> BV -> Bool
(BV n a) ==. (BV m b) = n == m && a == b

-- | Fixed-width not-equals
--
(/=.) :: BV -> BV -> Bool
u /=. v = not $ u ==. v

-- | Create a bit-vector.
--
-- /O(1)/
--
bitvec :: Integral a => Int -> a -> BV
bitvec n = BV n . fromIntegral
{-# INLINE bitvec #-}

-- | Create a mask of ones.
--
-- /O(1)/
--
ones :: Int -> BV
ones n = BV n $ 2^n - 1
{-# INLINE ones #-}

-- | Create a mask of zeros.
--
-- /O(1)/
--
zeros :: Int -> BV
zeros n = BV n 0
{-# INLINE zeros #-}

-- | Greatest natural number representable with @n@ bits
--
maxV :: Integral a => Int -> a
maxV n = 2^n - 1
{-# INLINE maxV #-}

-- | Concat two bit-vectors.
--
-- /O(1)/
--
(#) :: BV -> BV -> BV
(BV n a) # (BV m b) = BV (n + m) ((a `shiftL` m) + b)
{-# INLINABLE (#) #-}

zeroExtend :: Int -> BV -> BV
zeroExtend d (BV n a) = BV (n+d) a
{-# INLINE zeroExtend #-}

signExtend :: Int -> BV -> BV
signExtend d (BV n a)
  | testBit a (n-1) = BV (n+d) $ (maxV d `shiftL` n) + a
  | otherwise       = BV (n+d) a

foldl :: (a -> Bool -> a) -> a -> BV -> a
foldl f e (BV n a) = go (n-1) e
  where go i !x | i >= 0    = let !b = testBit a i in go (i-1) $ f x b
                | otherwise = x
{-# INLINE foldl #-}

foldr :: (Bool -> a -> a) -> a -> BV -> a
foldr f e (BV n a) = go 0 e
 where go i !x | i < n     = let !b = testBit a i in f b (go (i+1) x)
               | otherwise = x
{-# INLINE foldr #-}

reverse :: BV -> BV
reverse bv@(BV n _) = BV n $ snd $ foldl go (1,0) bv
  where go (v,acc) b | b         = (v',acc+v)
                     | otherwise = (v',acc)
          where v' = 2*v

-- | Bit indexing.
--
-- /O(1)/
--
(@.) :: BV -> Int -> Bool
(BV _ a) @. i = testBit a i
{-# INLINE (@.) #-}

-- | Bit-string extraction.
--
-- /O(1)/
--
(@@) :: BV -> (Int,Int) -> BV
(BV _ a) @@ (j,i) = BV m $ (a `shiftR` i) `mod` 2^m
  where m = j - i + 1

-- | Take least significant bits.
--
-- /O(1)/
--
least :: Int -> BV -> BV
least m (BV _ a) = BV m $ a `mod` 2^m

-- | Take most significant bits.
--
-- /O(1)/
--
most :: Int -> BV -> BV
most m (BV n a) = assert (m <= n) $
  BV m $ a `shiftR` (n-m)

instance Bits BitVector where
  (BV n1 a) .&. (BV n2 b) = BV n $ a .&. b
    where n = max n1 n2
  (BV n1 a) .|. (BV n2 b) = BV n $ a .|. b
    where n = max n1 n2
  (BV n1 a) `xor` (BV n2 b) = BV n $ a `xor` b
    where n = max n1 n2
  complement (BV n a) = BV n $ 2^n - 1 - a 
  bit i = BV (i+1) (2^i)
  testBit (BV n a) i | i < n     = testBit a i
                     | otherwise = False
  bitSize = undefined
  isSigned = const False
  shiftL (BV n a) k
    | k > n     = BV n 0
    | otherwise = BV n $ shiftL a k `mod` 2^n
  shiftR (BV n a) k
    | k > n     = BV n 0
    | otherwise = BV n $ shiftR a k
  rotateL bv       0 = bv
  rotateL (BV n a) k
    | k == n    = BV n a
    | k > n     = rotateL (BV n a) (k `mod` n)
    | otherwise = BV n $ h + l
    where s = n - k
          l = a `shiftR` s
          h = (a `shiftL` k) `mod` 2^n
  rotateR bv       0 = bv
  rotateR (BV n a) k
    | k == n    = BV n a
    | k > n     = rotateR (BV n a) (k `mod` n)
    | otherwise = BV n $ h + l
    where s = n - k
          l = a `shiftR` k
          h = (a `shiftL` s) `mod` 2^n


(<<) :: BV -> BV -> BV
bv@BV{width=n} << (BV _ k)
  | k >= fromIntegral n  = BV n 0
  | otherwise            = bv `shiftL` (fromIntegral k)
{-# INLINE (<<) #-}

(>>) :: BV -> BV -> BV
bv@BV{width=n} >> (BV _ k)
  | k >= fromIntegral n  = BV n 0
  | otherwise            = bv `shiftR` (fromIntegral k)
{-# INLINE (>>) #-}

(<<<) :: BV -> BV -> BV
bv@BV{width=n} <<< (BV _ k)
  | k >= n'   = bv `rotateL` (fromIntegral $ k `mod` n')
  | otherwise = bv `rotateL` (fromIntegral k)
  where n' = fromIntegral n
{-# INLINE (<<<) #-}

(>>>) :: BV -> BV -> BV
bv@BV{width=n} >>> (BV _ k)
  | k >= n'   = bv `rotateR` (fromIntegral $ k `mod` n')
  | otherwise = bv `rotateR` (fromIntegral k)
  where n' = fromIntegral n
{-# INLINE (>>>) #-}

replicate :: Int -> BV -> BV
replicate 1 u = u
replicate n u = u # replicate (n-1) u
