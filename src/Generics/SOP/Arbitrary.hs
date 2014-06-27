module Generics.SOP.Arbitrary (
    garbitrary
    -- * Re-exports
  , Arbitrary(..)
  ) where

import Control.Monad
import Data.Proxy
import Test.QuickCheck

import Generics.SOP

-- | Generic generation of Arbitrary values
--
-- Example usage:
--
-- > instance Arbitrary a => Show (Arbitrary a) where
-- >   arbitrary = garbitrary
--
-- Note how simple this function is. In this case, the function benefits from
-- the separatation of metadata because here metadata just distracts.
garbitrary :: forall a. (Generic a, All2 Arbitrary (Code a)) => Gen a
garbitrary = liftM to $ hsequence =<< elements (apInjs_POP $ hcpure p arbitrary)
  where
    p :: Proxy Arbitrary
    p = Proxy
