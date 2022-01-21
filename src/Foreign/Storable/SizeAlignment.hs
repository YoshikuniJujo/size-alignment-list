{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Foreign.Storable.SizeAlignment (
	module Foreign.Storable.SizeAlignment.Internal
	) where

import Foreign.Storable.SizeAlignment.Internal
import Foreign.Storable.SizeAlignment.TH

concat <$> instanceSizeAlignmentListTuple `mapM` [2 .. 7]
concat <$> instanceSizeAlignmentListUntilTuple `mapM` [2 .. 7]
