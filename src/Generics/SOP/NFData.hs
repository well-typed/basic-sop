module Generics.SOP.NFData (grnf) where

import Control.DeepSeq

import Generics.SOP

grnf :: (Generic a, All2 NFData (Code a)) => a -> ()
grnf = rnf . hcollapse . hcliftA p (K . rnf . unI) . from

p :: Proxy NFData
p = Proxy
