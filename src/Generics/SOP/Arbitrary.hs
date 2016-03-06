-- | Generic generation of random test cases.
--
-- This module contains a generic version of 'arbitrary' from the
-- "Test.Quickcheck" library, using @generics-sop@.
--
module Generics.SOP.Arbitrary (
    garbitrary
    -- * Re-exports
  , Arbitrary(..)
  ) where

import Control.Monad
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
--
-- Note that the size of arbitrary values is not very precise.  Divergence for
-- recursive structures is averted by scaling down child values by 10 points,
-- at the cost of potentially making parts of the structure smaller than
-- desired.
--
garbitrary :: forall a. (Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary = liftM to $ hsequence =<< elements subs
  where
    subs :: [SOP Gen (Code a)]
    subs = apInjs_POP (hcpure p (scale (\s -> max 0 (s - 10)) arbitrary))

    p :: Proxy Arbitrary
    p = Proxy
