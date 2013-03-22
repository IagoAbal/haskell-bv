{-# OPTIONS_GHC -fno-warn-orphans #-}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- |
-- Copyright : (c) 2012-2013 Iago Abal, HASLab & University of Minho
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>
--
-- QuickCheck properties for 'Data.BitVector'.
module Main where

import Data.BitVector

import Control.Applicative ( (<$>), (<*>) )

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property ( Property, Testable, forAll, (==>) )
import Test.QuickCheck.Gen

main :: IO ()
main = $(defaultMainGenerator)

c_MAX_SIZE :: Int
c_MAX_SIZE = 8192

data BV2 = BV2 !BV !BV
    deriving (Eq,Show)

data BV3 = BV3 !BV !BV !BV
    deriving (Eq,Show)

divides :: Integral a => a -> a -> Bool
divides k n = n `mod` k == 0

gSize :: Gen Int
gSize = min c_MAX_SIZE . (+1) . abs <$> arbitrary

gBV :: Int -> Gen BV
gBV sz = bitVec sz <$> choose (0::Integer,2^sz-1)

gDivisor :: Int -> Gen Int
gDivisor n = suchThat (choose (1,n)) (`divides` n)

forallDivisorOf :: Testable prop => Int -> (Int -> prop) -> Property
forallDivisorOf n = forAll (gDivisor n)

gIndex :: BV -> Gen Int
gIndex a = choose (0,size(a)-1)

forallIndexOf :: Testable prop => BV -> (Int -> prop) -> Property
forallIndexOf a = forAll (gIndex a)

gIndex1 :: BV -> Gen Int
gIndex1 a = choose (1,size a)

forallIndex1Of :: Testable prop => BV -> (Int -> prop) -> Property
forallIndex1Of a = forAll (gIndex1 a)

instance Arbitrary BV where
  arbitrary = gBV =<< gSize

instance Arbitrary BV2 where
  arbitrary = gSize >>= \sz -> BV2 <$> gBV sz <*> gBV sz

instance Arbitrary BV3 where
  arbitrary = gSize >>= \sz -> BV3 <$> gBV sz <*> gBV sz <*> gBV sz

-- * bitVec

prop_bv_nat :: Integer -> Property
prop_bv_nat i = i >= 0 ==> nat(fromInteger i) == i

prop_bv_neg :: Integer -> Property
prop_bv_neg i = i < 0 ==> int(fromInteger i) == i

-- * Indexing

prop_rev_index :: BV -> Property
prop_rev_index a = forallIndexOf a $ \i -> a !. i == a @. (size(a)-i-1)

prop_least :: BV -> Property
prop_least a = forallIndex1Of a $ \m -> least m a ==. a@@(m-1,0)

prop_most :: BV -> Property
prop_most a = forallIndex1Of a $ \m -> most m a ==. a@@(n-1,n-m)
  where n = size a

-- * Negate

prop_neg_id :: BV -> Bool
prop_neg_id a = -(-a) ==. a

prop_abs_id :: BV -> Bool
prop_abs_id a = abs(abs(a)) ==. abs(a)

-- * Addition

prop_plus_right_id :: BV -> Bool
prop_plus_right_id a = a + zeros(size a) ==. a

prop_plus_comm :: BV -> BV -> Bool
prop_plus_comm a b = a + b ==. b + a

prop_plus_assoc :: BV3 -> Bool
prop_plus_assoc (BV3 a b c) = (a + b) + c ==. a + (b + c)

-- * Multiplication

prop_mult_comm :: BV -> BV -> Bool
prop_mult_comm a b = a * b ==. b * a

prop_mult_assoc :: BV3 -> Bool
prop_mult_assoc (BV3 a b c) = (a * b) * c ==. a * (b * c)

prop_mult_plus_distrib :: BV3 -> Bool
prop_mult_plus_distrib (BV3 a b c) = a * (b + c) ==. (a * b) + (a * c)

-- * Division

prop_div :: BV -> BV -> Property
prop_div a b = b /= 0 ==> a == q*b + r && r <= b
  where (q,r) = quotRem a b

prop_sdiv_is_div :: BV -> BV -> Property
prop_sdiv_is_div a b =
  isNat a && isPos b ==> a `sdiv` b ==. a `div` b

prop_srem_is_rem :: BV -> BV -> Property
prop_srem_is_rem a b =
  isNat a && isPos b ==> a `srem` b ==. a `rem` b

prop_smod_is_rem :: BV -> BV -> Property
prop_smod_is_rem a b =
  isNat a && isPos b ==> a `smod` b ==. a `rem` b

-- * Not

prop_not_id :: BV -> Bool
prop_not_id a = not_(not_ a) ==. a

-- * And

prop_and_comm :: BV -> BV -> Bool
prop_and_comm a b = a .&. b ==. b .&. a

prop_and_assoc :: BV3 -> Bool
prop_and_assoc (BV3 a b c) = (a .&. b) .&. c ==. a .&. (b .&. c)

-- * Shift

prop_shl_id :: BV -> Bool
prop_shl_id a = a `shiftL` 0 ==. a

prop_shl_0 :: BV -> Int -> Property
prop_shl_0 a i = i >= size a ==> a `shiftL` i == 0

prop_shl_mul :: BV -> Property
prop_shl_mul a = forallIndex1Of a $ \i ->
                   a `shiftL` i == a * bitVec n ((2::Integer)^i)
  where n = size a

prop_shr_id :: BV -> Bool
prop_shr_id a = a `shiftR` 0 ==. a

prop_shr_0 :: BV -> Int -> Property
prop_shr_0 a i = i >= size a ==> a `shiftR` i == 0

prop_shr_div :: BV -> Property
prop_shr_div a = forallIndex1Of a $ \i ->
                   a `shiftR` i == a `div` bitVec n ((2::Integer)^i)
  where n = size a

-- * Rotate

prop_rol_id :: BV -> Bool
prop_rol_id a = a `rotateL` (size a) ==. a

prop_ror_id :: BV -> Bool
prop_ror_id a = a `rotateR` (size a) ==. a

-- * Split & group

prop_split_join_id :: BV -> Property
prop_split_join_id a = forallDivisorOf (size a) $ \n ->
  join (split n a) ==. a

prop_group_join_id :: BV -> Property
prop_group_join_id a = forallDivisorOf (size a) $ \n ->
  join (group_ n a) ==. a

