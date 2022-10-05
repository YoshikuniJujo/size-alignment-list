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

minimumAlignment :: Int
minimumAlignment = 256

sizeAlignmentTypeList ::
	forall (as :: [Type]) . MapTypeVal2 Storable as => [SizeAlignment]
-- sizeAlignmentTypeList = mapTypeVal2 @Storable @as (\x -> (sizeOf x, lcm minimumAlignment $ alignment x))
sizeAlignmentTypeList = mapTypeVal2 @Storable @as (\x -> (sizeOf x, alignment x))

class SizeAlignmentList a where
	sizeAlignmentList :: [SizeAlignment]

	default sizeAlignmentList :: (
		MapTypeVal2 Storable (Flatten (Rep a)) ) => [SizeAlignment]
	sizeAlignmentList = sizeAlignmentTypeList @(Flatten (Rep a))

sizeAlignmentTypeMaybeList ::
	forall (mas :: Maybe [Type]) . MapTypeValMaybe2 Storable mas =>
	Maybe [SizeAlignment]
sizeAlignmentTypeMaybeList =
	mapTypeValMaybe2 @Storable @mas (\x -> (sizeOf x, alignment x))

class SizeAlignmentListUntil t a where
	sizeAlignmentListUntil :: Maybe [SizeAlignment]

	default sizeAlignmentListUntil :: (
		MapTypeValMaybe2 Storable (Until t (Flatten (Rep a))) ) =>
		Maybe [SizeAlignment]
	sizeAlignmentListUntil =
		sizeAlignmentTypeMaybeList @(Until t (Flatten (Rep a)))

type MapStorableUntil t ts = MapTypeValMaybe2 Storable (Until t (Flatten (Rep ts)))
