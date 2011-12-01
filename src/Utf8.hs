-- | Test for performance of full UTF-8 roundtrip of 'String's via a packed
-- encoding.
module Utf8 where

-- TODO: Just use the Text package for decoding a lazy bytestring. Then unpack
-- it.
