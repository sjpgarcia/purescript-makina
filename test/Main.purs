module Test.Main where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign as Foreign
import Makina (class Makina, Step(..))
import Makina as Makina
import Partial.Unsafe (unsafeCrashWith)

data TreeF a n
  = Leaf a
  | Fork n n

instance Makina (TreeF a) where
  destructure = case _ of
    Leaf a ->
      { tag: "Leaf"
      , fields: Foreign.unsafeToForeign { a }
      , points: Foreign.unsafeToForeign []
      }
    Fork a b ->
      { tag: "Fork"
      , fields: Foreign.unsafeToForeign {}
      , points: Foreign.unsafeToForeign [ a, b ]
      }

  restructure { tag, fields, points } =
    case tag of
      "Leaf" ->
        let
          { a } = Foreign.unsafeFromForeign fields
        in
          Leaf a
      "Fork" ->
        case Foreign.unsafeFromForeign points of
          [ a, b ] ->
            Fork a b
          _ ->
            unsafeCrashWith "Failed pattern match at restructure."
      _ ->
        unsafeCrashWith "Failed pattern match at restructure."

mapDemo :: forall a b. (a -> b) -> TreeF Int a -> TreeF Int b
mapDemo f x = ST.run go
  where
  go :: forall r. ST r _
  go = do
    m <- Makina.create x

    initial <- m Nothing

    let
      aux :: _ -> ST r _
      aux (More current) = do
        next <- m $ Just $ f current
        aux next
      aux (Done result) =
        pure result

    aux initial

main :: Effect Unit
main = do
  case mapDemo (_ + 10) $ Leaf 1 of
    Leaf a ->
      log $ show a
    Fork a b -> do
      log $ show a
      log $ show b
  case mapDemo (_ + 10) $ Fork 2 3 of
    Leaf a ->
      log $ show a
    Fork a b -> do
      log $ show a
      log $ show b
