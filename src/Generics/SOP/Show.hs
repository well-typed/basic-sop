-- | Generic 'Show' instance
--
-- This is mostly just a proof of concept.
module Generics.SOP.Show (gshow) where

import Data.List (intercalate)

import Generics.SOP

-- | Generic show
gshow :: forall a. (Generic a, HasDatatypeInfo a, All2 Show (Code a))
      => a -> String
gshow a = case datatypeInfo (Proxy :: Proxy a) of
            ADT     _ _ cs -> gshow' cs         (from a)
            Newtype _ _ c  -> gshow' (c :* Nil) (from a)

gshow' :: (All2 Show xss, SingI xss) => NP ConstructorInfo xss -> SOP I xss -> String
gshow' cs (SOP sop) = unI . hcollapse $ hcliftA2' p goConstructor cs sop

goConstructor :: All Show xs => ConstructorInfo xs -> NP I xs -> K String xs
goConstructor (Constructor n) args =
    K $ intercalate " " (n : args')
  where
    args' :: [String]
    args' = hcollapse $ hcliftA p (K . show . unI) args

goConstructor (Record n ns) args =
    K $ n ++ " {" ++ intercalate ", " args' ++ "}"
  where
    args' :: [String]
    args' = hcollapse $ hcliftA2 p goField ns args

goField :: Show a => FieldInfo a -> I a -> K String a
goField (FieldInfo field) (I a) = K $ field ++ " = " ++ show a

p :: Proxy Show
p = Proxy
