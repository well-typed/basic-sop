-- | Generic show.
--
-- This module contains a generic show function defined using
-- @generics-sop@.
--
module Generics.SOP.Show (gshow) where

import Data.List (intercalate)

import Generics.SOP

-- | Generic show.
--
-- This function is a proof-of-concept implementation of a function
-- that is similar to the 'show' function you get by using
-- 'deriving Show'.
--
-- It serves as an example of an SOP-style generic function that makes
-- use of metadata. However, it does currently not handle parentheses
-- correctly, and is therefore not really usable as a replacement.
--
-- If you want to use it anyway on a datatype @T@ for which you have
-- a 'Generics.SOP.Generic' instance, you can use 'gshow' as follows:
--
-- > instance Show T where
-- >   show = gshow
--
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

goConstructor (Infix n _ _) (arg1 :* arg2 :* Nil) =
    K $ show arg1 ++ " " ++ show n ++ " " ++ show arg2
goConstructor (Infix _ _ _) _ = error "inaccessible"

goField :: Show a => FieldInfo a -> I a -> K String a
goField (FieldInfo field) (I a) = K $ field ++ " = " ++ show a

p :: Proxy Show
p = Proxy
