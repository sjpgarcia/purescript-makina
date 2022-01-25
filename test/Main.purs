module Test.Main where

import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Control.Monad.Gen (chooseInt)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array ((..))
import Data.Functor.Mu (Mu(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign as Foreign
import Makina (class Makina)
import Makina as Makina
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen)

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
            unsafeCrashWith "Unrecognized points."
      _ ->
        unsafeCrashWith "Unrecognized tag."

mapDemo :: forall a b. (a -> b) -> TreeF Int a -> TreeF Int b
mapDemo f x = ST.run go
  where
  go :: forall r. ST r _
  go = do
    m <- Makina.create x

    initial <- m Nothing

    let
      aux :: _ -> ST r _
      aux (Makina.More current) = do
        next <- m $ Just $ f current
        aux next
      aux (Makina.Done result) =
        pure result

    aux initial

type Algebra f a = f a -> a

cata :: forall f a. Makina f => Algebra f a -> Mu f -> a
cata algebra (In initialStructure) = ST.run go
  where
  go :: forall r. ST r _
  go = do
    initialFeed <- Makina.create initialStructure
    initialIndex <- initialFeed Nothing
    tailRecM loop { feed: initialFeed, index: initialIndex, stack: Nil }
    where
    loop :: _ -> ST r _
    loop { feed, index, stack } =
      case index of
        Makina.More (In nextStructure) -> do
          nextFeed <- Makina.create nextStructure
          nextIndex <- nextFeed Nothing
          pure $ Loop { feed: nextFeed, index: nextIndex, stack: feed : stack }
        Makina.Done doneResult ->
          case stack of
            prevFeed : stackTail -> do
              prevIndex <- prevFeed $ Just $ algebra doneResult
              pure $ Loop { feed: prevFeed, index: prevIndex, stack: stackTail }
            Nil ->
              pure $ Done $ algebra doneResult

cataDemo :: Mu (TreeF Int) -> Int
cataDemo = cata algebra
  where
  algebra (Leaf a) = a
  algebra (Fork a b) = a + b

leaf :: forall a. a -> Mu (TreeF a)
leaf a = In (Leaf a)

fork :: forall a. Mu (TreeF a) -> Mu (TreeF a) -> Mu (TreeF a)
fork a b = In (Fork a b)

treeOf ∷ ∀ a. Int → Gen a → Gen (Mu (TreeF a))
treeOf n g
  | n <= 0 = g <#> \x → leaf x
  | otherwise = g >>= \x → tailRecM go (Tuple (leaf x) n)
      where
      go (Tuple a 0) = pure (Done a)
      go (Tuple a c) = do
        l ← chooseInt 0 3
        r ← chooseInt 0 3

        l' ←
          if l == 0 then g <#> \x → leaf x
          else pure (fork a a)

        r' ←
          if r == 0 then g <#> \x → leaf x
          else pure (fork a a)

        pure $ Loop (Tuple (fork l' r') (c - 1))

foldingTree ∷ Benchmark
foldingTree = mkBenchmark
  { slug: "foldingTree"
  , title: "Integer summation"
  , sizes: (1 .. 10) <#> (_ * 1)
  , sizeInterpretation: "Approximate Depth"
  , inputsPerSize: 10
  , gen: \n → treeOf n arbitrary
  , functions:
      [ benchFn "cataDemo" cataDemo
      ]
  }

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
  log $ show $ cataDemo (fork (fork (leaf 10) (leaf 11)) (fork (leaf 10) (leaf 11)))
  runSuite [ foldingTree ]
