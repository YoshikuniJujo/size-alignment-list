{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE KindSignatures, DataKinds, ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.SizeAlignment.Internal (
	SizeAlignmentList(sizeAlignmentList),
	SizeAlignmentListUntil(sizeAlignmentListUntil), MapStorableUntil,
	Size, Alignment, SizeAlignment ) where

import GHC.Generics
import GHC.Generics.TypeFam
import Foreign.Storable
import Data.Kind
import Data.Type.TypeFam
import Data.Type.TypeValMap

type Size = Int
type Alignment = Int
type SizeAlignment = (Size, Alignment)

sizeAlignmentTypeList ::
	forall (as :: [Type]) . MapTypeVal2 Storable as => [SizeAlignment]
sizeAlignmentTypeList = mapTypeVal2 @Storable @as (\x -> (sizeOf x, alignment x))

class SizeAlignmentList a where
	sizeAlignmentList :: [SizeAlignment]

	default sizeAlignmentList :: (
		Generic a,
		MapTypeVal2 Storable (Flatten (Rep a)) ) => [SizeAlignment]
	sizeAlignmentList = sizeAlignmentTypeList @(Flatten (Rep a))

class SizeAlignmentListUntil t a where
	sizeAlignmentListUntil :: [SizeAlignment]

	default sizeAlignmentListUntil :: (
		Generic a,
		MapTypeVal2 Storable (FromJust (Until t (Flatten (Rep a)))) ) =>
		[SizeAlignment]
	sizeAlignmentListUntil =
		sizeAlignmentTypeList @(FromJust (Until t (Flatten (Rep a))))

type MapStorableUntil t ts = MapTypeVal2 Storable (FromJust (Until t ts))
