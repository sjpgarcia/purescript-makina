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
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Foreign (Foreign)
import Makina (class Makina)
import Makina as Makina
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen)
import Unsafe.Coerce (unsafeCoerce)

data TreeF a n
  = Leaf a
  | Fork n n

instance Makina (TreeF a) where
  destructure = case _ of
    Leaf a ->
      { tag: "Leaf"
      , fields: unsafeCoerce { a }
      , points: unsafeCoerce []
      }
    Fork a b ->
      { tag: "Fork"
      , fields: unsafeCoerce {}
      , points: unsafeCoerce [ a, b ]
      }

  restructure { tag, fields, points } =
    case tag of
      "Leaf" ->
        let
          { a } = unsafeCoerce fields
        in
          Leaf a
      "Fork" ->
        case unsafeCoerce points of
          [ a, b ] ->
            Fork a b
          _ ->
            unsafeCrashWith "Unrecognized points."
      _ ->
        unsafeCrashWith "Unrecognized tag."

type Algebra f a = f a -> a

foreign import cataImpl :: Foreign

cata :: forall f a. Makina f => Algebra f a -> Mu f -> a
cata =
  (unsafeCoerce cataImpl)
    (Makina.create :: f a -> _)
    Makina.init
    Makina.step

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
  runSuite [ foldingTree ]
