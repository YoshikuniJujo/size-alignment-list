{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.SizeAlignment.TH (
	instanceSizeAlignmentListTuple,
	instanceSizeAlignmentListUntilTuple ) where

import Language.Haskell.TH
import Foreign.Storable
import Foreign.Storable.SizeAlignment.Internal

instanceSizeAlignmentListTuple :: Int -> Q Dec
instanceSizeAlignmentListTuple n = newNames n >>= \ns -> instanceD
	(cxt $ (conT ''Storable `appT`) . varT <$> ns)
	(conT ''SizeAlignmentList `appT` tupT (varT <$> ns)) []

tupT :: [TypeQ] -> TypeQ
tupT ts = foldl appT (tupleT $ length ts) ts

newNames :: Int -> Q [Name]
newNames n = newName `mapM` take n ((: "") <$> ['a' .. 'z'])

instanceSizeAlignmentListUntilTuple :: Int -> Q Dec
instanceSizeAlignmentListUntilTuple n =
	newName "t" >>= \t -> newNames n >>= \ns -> instanceD
		(cxt [conT ''MapStorableUntil `appT` varT t `appT` promotedListT (varT <$> ns)])
		(conT ''SizeAlignmentListUntil `appT` varT t `appT` tupT (varT <$> ns)) []

promotedListT :: [TypeQ] -> TypeQ
promotedListT = foldr (\p ps -> promotedConsT `appT` p `appT` ps) promotedNilT
