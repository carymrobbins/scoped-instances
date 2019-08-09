{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module ScopedInstances where

import Data.Coerce
import Data.Kind (Constraint)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as Text

newtype Encoder a = Encoder
  { runEncoder :: a -> Text }

class Encode a where
  encode :: a -> Text

showEncoder :: (Show a) => Encoder a
showEncoder = Encoder $ Text.pack . show

newtype ShowEncode a = ShowEncode { unShowEncode :: a }

instance (Show a) => Encode (ShowEncode a) where
  encode = runEncoder showEncoder . unShowEncode

intEncoder :: Encoder Int
intEncoder = showEncoder

deriving via ShowEncode Int instance Encode Int

floatEncoder :: Encoder Float
floatEncoder = showEncoder

deriving via ShowEncode Float instance Encode Float

textEncoder :: Encoder Text
textEncoder = Encoder id

instance Encode Text where
  encode = runEncoder textEncoder

-- TODO: -Wsimplifiable-class-constraints complains about fragile inner bindings
genericEncoder :: forall a. (Encode (GenericEncode a)) => Encoder a
genericEncoder = Encoder $ (encode @(GenericEncode a)) . GenericEncode

newtype GenericEncode a = GenericEncode { unGenericEncode :: a }

instance (Encode (f p)) => Encode (M1 D x f p) where
  encode (M1 x) = encode @(f p) x

instance (Encode (f p)) => Encode (M1 C x f p) where
  encode (M1 x) = encode @(f p) x

instance (Encode t, Selector s) => Encode (M1 S s (K1 R t) p) where
  encode m@(M1 (K1 x)) = Text.pack (selName m) <> " = " <> encode x

instance (Encode (a p), Encode (b p)) => Encode ((a :*: b) p) where
  encode (a :*: b) = (encode @(a p) a) <> ", " <> (encode @(b p) b)

instance (Generic a, Encode (Rep a p)) => Encode (GenericEncode a) where
  encode = (encode @(Rep a p)) . from . unGenericEncode

-- TODO: Copy pasta to support Using/As stuff. Can probably be better
-- generalized, maybe with a type-level list?
-- Something like -
-- {{{
--    GenericEncode Rec `Using` '[ Encode (Text `As` Uptext) ]
-- }}}
-- Then we can just define the vanilla GenericEncode instance in terms of
-- {{{
--    GenericEncode Rec `Using` '[]
-- }}}

instance (Encode (f p `Using` Encode (a `As` b)))
  => Encode (M1 D x f p `Using` Encode (a `As` b))
  where
  encode (Using (M1 x)) = encode @(f p `Using` Encode (a `As` b)) (Using x)

instance (Encode (f p `Using` Encode (a `As` b)))
  => Encode (M1 C x f p `Using` Encode (a `As` b))
  where
  encode (Using (M1 x)) = encode @(f p `Using` Encode (a `As` b)) (Using x)

instance
  ( Encode (t `Using` Encode (a `As` b))
  , Selector s
  ) => Encode (M1 S s (K1 R t) p `Using` Encode (a `As` b))
  where
  encode (Using m@(M1 (K1 x))) =
    Text.pack (selName m)
      <> " = " <>
      encode @(t `Using` Encode (a `As` b)) (Using x)

instance {-# OVERLAPPING #-}
  ( Encode b
  , Coercible a b
  ) => Encode (a `Using` Encode (a `As` b))
  where
  encode (Using a) = encode @b (coerce a)

instance {-# OVERLAPPABLE #-}
  (Encode c) => Encode (c `Using` Encode (a `As` b))
  where
  encode (Using c) = encode @c c

instance
  ( Encode (a p `Using` Encode (c `As` d))
  , Encode (b p `Using` Encode (c `As` d))
  ) => Encode ((a :*: b) p `Using` Encode (c `As` d)) where
  encode (Using (a :*: b)) =
    (encode @(a p `Using` Encode (c `As` d)) (Using a))
      <> ", " <>
      (encode @(b p `Using` Encode (c `As` d)) (Using b))

instance
  ( Generic a
  , Encode (Rep a p `Using` Encode (c `As` d))
  ) => Encode (GenericEncode a `Using` Encode (c `As` d)) where
  encode (Using x) =
    (encode @(Rep a p `Using` Encode (c `As` d)))
      $ Using @(Rep a p) @(Encode (c `As` d))
      $ from @a
      $ unGenericEncode x

newtype a `Using` (x :: Constraint) = Using a
  deriving stock (Show)

data a `As` b = As
  deriving stock (Show)
