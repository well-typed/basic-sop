-- | Generic equality.
--
-- This module contains a generic equality function defined using
-- @generics-sop@.
--
module Generics.SOP.Eq (geq) where

import Data.Function
import Generics.SOP

-- | Generic equality.
--
-- This function reimplements the built-in generic equality that
-- you get by using @deriving Eq@.
--
-- Assuming you have a 'Generics.SOP.Generic' instance for a
-- datatype @T@, you can use 'geq' as follows:
--
-- > instance Eq T where
-- >   (==) = geq
--
geq :: (Generic a, All2 Eq (Code a)) => a -> a -> Bool
geq = go `on` from
  where
    go :: forall xss. (All2 Eq xss, All SListI xss) => SOP I xss -> SOP I xss -> Bool
    go (SOP (Z xs))  (SOP (Z ys))  = and . hcollapse $ hcliftA2 p eq xs ys
    go (SOP (S xss)) (SOP (S yss)) = go (SOP xss) (SOP yss)
    go _             _             = False

    p :: Proxy Eq
    p = Proxy

    eq :: forall (a :: *). Eq a => I a -> I a -> K Bool a
    eq (I a) (I b) = K (a == b)
