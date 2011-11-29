Lately, I was thinking about how to improve the speed of the 'binary'
serialization library. There are two main improvement vectors.

1. We can improve the datastructures/abstractions used to encode and decode
   binary data.

2. We can change the serialization format to allow for faster encoding and
   decoding.

Note that the improvements may come at the cost of compatibility with the
existing API. If it warrants significant speed improvments, then that is fine
in my opinion.

In earlier posts, I focused on the lazy bytestring 'Builder' abstraction,
which allows to implement efficient encodings, i.e., functions mapping Haskell
values to sequences of bytes. The blaze-builder library provided an early
prototype of a lazy bytestring builder. Recently, I have completed a
significant rewrite of it, which will be available in the next release of the
bytestring library.

In this post, I provide benchmark results that allow a more informed choice for
the design and implementation of the decoding side of a 'binary' serialization
library. The baseline is provided by the parser implemented in the 'Get' monad
of the current 'binary-0.5.0.2' library, which is called 'binaryget' in the
following benchmarks. This parser is a simple state monad over the input
represented as a lazy bytestring. It does not support failure recovery. An
obvious competition is the bytestring parser provided by the 'attoparsec' libary.
It is implemented using continuation-passing-style (CPS) and supports 
failure recovery and partial parses (enumerator style parsing). 

Note that failure recovery is not required to parse a binary serialization
format. 


Scrapbook
---------

Design proposal for a new binary serialization format:

1. Use a parser design (similar to attoparsec) that supports partial parses.
   Use only a 

similar to attoparsec. Howev

1. Use chunked encoding for bounded types. Decoding is then factored into
   extracting a lazy bytestring from the chunk lengths and decoding this
   lazy bytestring efficiently.

2. Use a tagged encoding for recursive types.

-}
--
--
-- 2. There are two different efficiency problems:
--      1. Ensure that one pays for backtracking only if one uses it.
--      2. Ensure that bounded types are parsed efficiently; i.e., if there
--         is enough input remaining straight-line IO code should be used.
--
-- 3. Fine-tuning will require that special attentation is paid to strictness
--    issues. They should however not be too difficult.
--    

