module Makina where

import Prelude

import Foreign (Foreign)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

type Info =
  { tag :: String, fields :: Foreign, points :: Foreign }

class Makina (f :: Type -> Type) where
  destructure :: forall i. f i -> Info
  restructure :: forall i. Info -> f i

foreign import createImpl
  :: Foreign
  -> (Foreign -> Info)
  -> (Info -> Foreign)
  -> (Foreign -> Foreign)

create :: forall f a b. Makina f => f a -> (Message b -> (Result f a b))
create structure =
  unsafeCoerce $
    createImpl
      (unsafeCoerce structure)
      (unsafeCoerce (destructure :: f a -> Info))
      (unsafeCoerce (restructure :: Info -> f a))

data Message :: forall k. k -> Type
data Message b

init :: forall b. Message b
init = unsafeCoerce { tag: "init" }

step :: forall b. b -> Message b
step a = unsafeCoerce { tag: "step", value: a }

data Result :: forall k1 k2. (k1 -> Type) -> k1 -> k2 -> Type
data Result f a b

onResult :: forall f a b c. (a -> c) -> (f b -> c) -> Result f a b -> c
onResult onMore onDone = unsafeCoerce $ case _ of
  { tag: "more", value } -> onMore (unsafeCoerce value)
  { tag: "done", value } -> onDone (unsafeCoerce value)
  _ ->
    unsafeCrashWith "Internal Error."
