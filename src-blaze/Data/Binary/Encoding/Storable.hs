
-- |
-- Module      : Data.Binary.Encoding.Storable
-- Copyright   : (c) 2010 Simon Meier
--
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's and 'Builder's for serializing storable values.
--
module Data.Binary.Encoding.Storable
    ( 
    -- * Writing 'Storable's
      writeStorable
    , fromStorable
    , fromStorables

    ) where

import Data.Binary.Builder.Internal

import Foreign

------------------------------------------------------------------------------
-- Writing storables
------------------------------------------------------------------------------

-- | Write a storable value.
writeStorable :: Storable a => a -> Write 
writeStorable x = exactWrite (sizeOf x) (\op -> poke (castPtr op) x)

-- | A builder that serializes a storable value. No alignment is done.
fromStorable :: Storable a => a -> Builder
fromStorable = fromWriteSingleton writeStorable

-- | A builder that serializes a list of storable values by writing them
-- consecutively. No alignment is done. 
fromStorables :: Storable a => [a] -> Builder
fromStorables = fromWriteList writeStorable
