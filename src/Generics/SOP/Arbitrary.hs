-- | Generic generation of random test cases.
--
-- This module contains a generic version of 'arbitrary' from the
-- "Test.Quickcheck" library, using @generics-sop@.
--
module Generics.SOP.Arbitrary (
    garbitrary
  , gshrink
    -- * Re-exports
  , Arbitrary(..)
  ) where

import Test.QuickCheck

import Generics.SOP

-- | Generic generation of random test cases.
--
-- This function is a proof-of-concept implementation of a generic
-- 'arbitrary' that can be used to instantiate the 'Arbitrary' class
-- in @QuickCheck@.
--
-- If you want to use it on a datatype @T@ for which you have a
-- 'Generics.SOP.Generic' instance, you can say:
--
-- > instance Arbitrary T where
-- >   arbitrary = garbitrary
-- >   shrink    = gshrink
--
-- Note that currently no attempts are being made to generate arbitrary
-- values of a particular size, and it is possible that this function
-- diverges for recursive structures.
--
garbitrary :: forall a. (Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary = fmap to $ do
    genSopGen <- elements (apInjs_POP $ hcpure p arbitrary) -- :: Gen (SOP Gen (Code a))
    hsequence genSopGen
  where
    p :: Proxy Arbitrary
    p = Proxy

-- | Generic shrink. See 'garbitrary'.
--
-- Note that this doesn't work very well for recursive structures:
--
-- >>> sort $ shrink ([1,2] :: [Int])
-- [[],[0,2],[1],[1,0],[1,1],[2]]
--
-- >>> sort $ gshrink ([1,2] :: [Int])
-- [[0,2],[1],[1,0],[1,1]]
--
gshrink :: forall a. (Generic a, All2 Arbitrary (Code a)) => a -> [a]
gshrink
    = map (to . SOP)
    . hsequence'
    . hcmap (Proxy :: Proxy (All Arbitrary)) gshrink'
    . unSOP
    . from
  where
    gshrink' :: forall ys. All Arbitrary ys => NP I ys -> ([] :.: NP I) ys
    gshrink' Nil          = Comp $ []
    gshrink' (I x :* Nil) = Comp $ fmap (\x' -> I x' :* Nil) $ shrink x
    gshrink' (I x :* xs)  = Comp $
        [ I x' :* xs  | x'  <- shrink x ] ++
        [ I x  :* xs' | xs' <- unComp $ gshrink' xs ]
