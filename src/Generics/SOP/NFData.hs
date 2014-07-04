-- | Generic reduction to normal form.
--
-- This module contains a generic function that reduces
-- a value to normal form, defined using @generics-sop@.
--
module Generics.SOP.NFData (grnf) where

import Control.DeepSeq

import Generics.SOP

-- | Generic reduction to normal form.
--
-- This function is a generic implementation of the
-- 'rnf' function that can be used to instantiate the
-- 'NFData' class in @deepseq@.
--
-- Assuming you have a 'Generics.SOP.Generic' instance
-- for your datatype @T@, you can use 'grnf' as follows:
--
-- > instance NFData T where
-- >   rnf = grnf
--
grnf :: (Generic a, All2 NFData (Code a)) => a -> ()
grnf = rnf . hcollapse . hcliftA p (K . rnf . unI) . from

p :: Proxy NFData
p = Proxy
