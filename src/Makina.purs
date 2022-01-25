module Makina where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Ref as STRef
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Foreign as Foreign

class Makina (f :: Type -> Type) where
  destructure :: forall i. f i -> { tag :: String, fields :: Foreign, points :: Foreign }
  restructure :: forall i. { tag :: String, fields :: Foreign, points :: Foreign } -> f i

create :: forall f a b r. Makina f => f a -> ST r (Maybe b -> ST r (Step f a b))
create t = do
  let { tag, fields, points } = destructure t
  let
    inputPoints :: Array a
    inputPoints = Foreign.unsafeFromForeign points

  inputStack <- STRef.new $ List.fromFoldable inputPoints
  outputStack <- STRef.new Nil

  pure $ \maybeInput -> do
    case maybeInput of
      Just input -> do
        void $ STRef.modify (input : _) outputStack
      Nothing ->
        pure unit
    inputStack' <- STRef.read inputStack
    case inputStack' of
      Nil -> do
        outputStack' <- List.reverse <$> STRef.read (outputStack)
        let
          outputPoints = Foreign.unsafeToForeign (List.toUnfoldable outputStack' :: Array b)
        pure $ Done $ restructure { tag, fields, points: outputPoints }
      head : tail -> do
        _ <- STRef.write tail inputStack
        pure $ More head

data Step :: (Type -> Type) -> Type -> Type -> Type
data Step f i o
  = More i
  | Done (f o)
