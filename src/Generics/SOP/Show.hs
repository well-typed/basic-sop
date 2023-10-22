-- | Generic show.
--
-- This module contains a generic show function defined using
-- @generics-sop@.
--
module Generics.SOP.Show (gshowsPrec, gshow) where

import Data.List (intersperse)

import Generics.SOP

-- | Generic show.
--
-- This function is a proof-of-concept implementation of a function
-- that is similar to the 'show' function you get by using
-- 'deriving Show'.
--
-- It serves as an example of an SOP-style generic function that makes
-- use of metadata.
--
-- If you want to use it on a datatype @T@ for which you have a
-- 'Generics.SOP.Generic' instance, you can use 'gshowsPrec' as
-- follows:
--
-- > instance Show T where
-- >   showsPrec = gshowsPrec
--
gshowsPrec :: forall a. (Generic a, HasDatatypeInfo a, All2 Show (Code a))
           => Int -> a -> ShowS
gshowsPrec prec a =
  gshowsPrec' prec (constructorInfo (datatypeInfo (Proxy :: Proxy a))) (from a)

gshow :: (Generic a, HasDatatypeInfo a, All2 Show (Code a)) => a -> String
gshow a = gshowsPrec 0 a ""

gshowsPrec' :: (All2 Show xss, SListI xss) => Int -> NP ConstructorInfo xss -> SOP I xss -> ShowS
gshowsPrec' prec cs (SOP sop) =
  hcollapse $ hcliftA2 allp (goConstructor prec) cs sop

goConstructor :: All Show xs => Int -> ConstructorInfo xs -> NP I xs -> K ShowS xs
goConstructor prec (Constructor n) args =
    K $
      showParen
        (fixity <= prec)
        (foldr (.) id $ intersperse (showString " ") (showString n : args'))
  where
    args' :: [ShowS]
    args' = hcollapse $ hcliftA p (K . showsPrec 11 . unI) args

    -- With fixity = 11 the parens will be shown only if the enclosing
    -- context is a function application.  This is correct because
    -- function application is the only thing that binds tightly
    -- enough to force parens around this expression.
    fixity = 11

goConstructor prec (Record n ns) args =
    K $
      showParen
        (fixity <= prec)
        (showString n . showString " {" . foldr (.) id (intersperse (showString ", ") args') . showString "}")
  where
    args' :: [ShowS]
    args' = hcollapse $ hcliftA2 p goField ns args

    -- With fixity = 12 the parens will never be shown.  This is
    -- correct because record construction binds tighter than even
    -- function application!
    fixity = 12

goConstructor prec (Infix n _ fixity) (I arg1 :* I arg2 :* Nil) =
    K $
      showParen
        (fixity <= prec)
        (showsPrec fixity arg1 . showString " " . showString n . showString " " . showsPrec fixity arg2)
#if __GLASGOW_HASKELL__ < 800
goConstructor _ (Infix _ _ _) _ = error "inaccessible"
#endif

goField :: Show a => FieldInfo a -> I a -> K ShowS a
goField (FieldInfo field) (I a) = K $ showString field . showString " = " . showsPrec 0 a

p :: Proxy Show
p = Proxy

allp :: Proxy (All Show)
allp = Proxy
