{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.SizeAlignment.TH (
	instanceSizeAlignmentListTuple,
	instanceSizeAlignmentListUntilTuple ) where

import Language.Haskell.TH
import Foreign.Storable
import Foreign.Storable.SizeAlignment.Internal
import Data.Bool

instanceSizeAlignmentListTuple :: Int -> DecsQ
instanceSizeAlignmentListTuple n = newTypes n >>= \ts -> do
	let	tpl = tupT ts
	(isInstance ''SizeAlignmentList . (: []) =<< tpl) >>= bool
		((: []) <$> instanceD
			(cxt $ (conT ''Storable `appT`) <$> ts)
			(conT ''SizeAlignmentList `appT` tpl) [])
		(pure [])

tupT :: [TypeQ] -> TypeQ
tupT ts = foldl appT (tupleT $ length ts) ts

newTypes :: Int -> Q [TypeQ]
newTypes = ((varT <$>) <$>) . newNames

newNames :: Int -> Q [Name]
newNames n = newName `mapM` take n ((: "") <$> ['a' .. 'z'])

instanceSizeAlignmentListUntilTuple :: Int -> DecsQ
instanceSizeAlignmentListUntilTuple n =
	newName "t" >>= \t -> newTypes n >>= \ts -> do
		let	tpl = tupT ts
		(isInstance ''SizeAlignmentListUntil . (: []) =<< tpl) >>= bool
			((: []) <$> instanceD
				(cxt [conT ''MapStorableUntil `appT`
					varT t `appT` promotedListT ts])
				(conT ''SizeAlignmentListUntil `appT`
					varT t `appT` tpl) [])
			(pure [])

promotedListT :: [TypeQ] -> TypeQ
promotedListT = foldr (\p ps -> promotedConsT `appT` p `appT` ps) promotedNilT
