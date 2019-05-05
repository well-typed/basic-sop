-- | Generic computation of a skeleton.
module Generics.SOP.Skeleton (
  -- $skeleton
  Skeleton(..)
  ) where

import Control.Exception
import Data.Text (Text)

import Generics.SOP

-- $skeleton
--
-- A skeleton for a record type has a defined "spine" but is undefined
-- everywhere else. For instance, a skeleton for pairs would be
--
-- > (undefined, undefined)

-- | Generic computation of a skeleton.
--
-- A skeleton for a record type has a defined "spine" but is undefined
-- everywhere else. For instance, a skeleton for pairs would be
--
-- > (undefined, undefined)
--
-- We introduce a type class for this purpose because the skeleton for nested
-- records would look like
--
-- > (undefined, (undefined, undefined))
--
-- The default instance of 'skeleton' applies to record types; for everything
-- else, use undefined (or error):
--
-- > instance Skeleton SomeRecordType -- no where clause
--
-- or
--
-- > instance Skeleton SomeNonRecordType where skeleton = undefined
--
-- This is an example of how SOP-style generic functions can
-- be used with @DefaultSignatures@.
--
-- Furthermore, metadata is used in order to produce better
-- error messages. For the undefined components of a record,
-- an error is triggered that mentions the name of the field.
--
class Skeleton a where
  default skeleton :: (Generic a, HasDatatypeInfo a, Code a ~ '[xs], All Skeleton xs) => a

  -- | Returns a skeleton.
  skeleton :: a
  skeleton = gskeleton

instance Skeleton [a]       where skeleton = undefined
instance Skeleton (Maybe a) where skeleton = undefined

instance Skeleton Int       where skeleton = undefined
instance Skeleton Double    where skeleton = undefined
instance Skeleton Rational  where skeleton = undefined
instance Skeleton Bool      where skeleton = undefined
instance Skeleton Text      where skeleton = undefined

{-------------------------------------------------------------------------------
  Generic instance
-------------------------------------------------------------------------------}

-- | Compute a "spine" for a single constructor datatype. That is, a valid value of
-- that type with a defined spine but undefined everywhere else. For record
-- types we give "error" values that mention the names of the fields.
gskeleton :: forall a xs. (Generic a, HasDatatypeInfo a, Code a ~ '[xs], All Skeleton xs) => a
gskeleton = to $ gskeleton' (datatypeInfo (Proxy :: Proxy a))

gskeleton' :: All Skeleton xs => DatatypeInfo '[xs] -> SOP I '[xs]
gskeleton' d = gskeletonFor (hd (constructorInfo d))
#if __GLASGOW_HASKELL__ < 800
gskeleton' _ = error "inaccessible"
#endif

gskeletonFor :: All Skeleton xs => ConstructorInfo xs -> SOP I '[xs]
gskeletonFor (Constructor _)     = SOP $ Z $ spineWithNames (hpure (K ""))
gskeletonFor (Infix       _ _ _) = SOP $ Z $ spineWithNames (hpure (K ""))
gskeletonFor (Record      _ fs)  = SOP $ Z $ spineWithNames (hliftA sfieldName fs)
  where
    sfieldName :: FieldInfo a -> K String a
    sfieldName (FieldInfo n) = K n

spineWithNames :: (All Skeleton xs, SListI xs) => NP (K String) xs -> NP I xs
spineWithNames = hcliftA ps aux
  where
    aux :: Skeleton a => K String a -> I a
    aux (K "") = I $ skeleton
    aux (K n)  = I $ mapException (addFieldName n) skeleton

addFieldName :: FieldName -> ErrorCall -> ErrorCall
#if MIN_VERSION_base(4,9,0)
addFieldName n (ErrorCallWithLocation str loc) =
  ErrorCallWithLocation (n ++ ": " ++ str) loc
#else
addFieldName n (ErrorCall str) = ErrorCall (n ++ ": " ++ str)
#endif

ps :: Proxy Skeleton
ps = Proxy
