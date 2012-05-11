Overview
========

Projects such as cloud-haskell or acid-state crucially rely on performant,
generic serialization. As I've developed the new bytestring builder, I
wondered what speedup I could gain using it for encoding a Haskell value to an
unambiguous binary representation. This library, `blaze-binary`, is the
(current) result state of this experiment.

In preliminary benchmarks on my i7, 64bit Linux machine, this library is 2 - 4
times faster for binary encoding than both `binary-0.5.0.2` and
`cereal-0.3.5.1`. Decoding is as fast as `binary-0.5.0.2`, but allows feeding
the input in a chunkwise fashion, like `attoparsec`. Our decoder is at least
2x faster than using attoparsec directly.

As an additional improvement over binary and cereal, this library can
also output a textual representation of the sequence of primitive values
(e.g., `Int`s, `Double`s, and strict `ByteString`s). Moreover, the encoding
for this stream of primitive values can be chosen at runtime without any
performance impact. This allows for example a developer of a CloudHaskell
application to analyse the messages sent and received without having access to
the type of the data being sent. This is especially interesting for displaying
error messages in the case of a failed parse of a received message or to
investigate the communication patterns of a running CloudHaskell application.


Encoding Implementation
=======================

The implementation uses a two step approach to encode a Haskell value to a
sequence of bytes.  In a first step, the Haskell value is converted to a
stream of primitive values, where a primitive value is an `IntX` or `WordX`
for `X` in `["", "8","16","32","64"]`, a `Float`, `Double`, `Integer`, a
`ByteString`, or a `Text` value. The conversion uses a difference list
representation of the primitive stream to ensure *O(1)*-concatentation.  In
the second step, the stream of primitive values is converted to a sequence of
bytes using the new bytestring builder and its support for bounded encodings. 

This splitting of the encoding into a "flattening pass" and an "primtive
encoding pass" results in the nice benefit that the encoding of the stream of
primitive values can be chosen at runtime. Morover, it is more efficient, as
the benchmarks demonstrate. In the beginning, I implemented a version that
encodes the values directly using the new bytestring builder. This initial
version did not result in any speedup with respect to binary and cereal. My
current hypothesis is that the type of all of these builders leads to too many
unknown and possibly even unsaturated calls, whereas the difference list for
the stream of primitive values only results in calls to unknown THUNKs.
Evaluating unknown THUNKs is the fastest unkwon call.


High-level Encoding Format
==========================

In contrast to binary and cereal, this library encodes lists in a streaming
fashion, tagging `(:)` with 1 and `[]` with 0. This results in only one pass
through a list and reduces GC pressure as it retains less memory than the list
serialization used by binary and cereal, which prefixes the list with the
number of elements.

We also do not use the `Put` monad. The monadic value-passing is just not
required.


Encoding Primitive Values
=========================


I plan to implement two different encoding formats: one format optimized for
compactness and one optimized for throughput. Both of these formats come in a
tagged variant that allows decoding the stream of primitive values without
access to the type.

All results are prefixed with a 4-byte identifier. Currently, we use the
following assignment of identifiers to formats.

    0xce,0xbb,0x2e,0x30    throughput, untagged
    0xce,0xbb,0x2e,0x31    throughput, tagged
    0xce,0xbb,0x2e,0x32    compact, untagged
    0xce,0xbb,0x2e,0x33    compact, tagged

The compact and the throughput format only differ in how they encode `IntX`s,
`WordX`s, and `Integer`s. For the common primitive values they use the
following encodings.

  - `Char`s are UTF-8 encoded.
  - `Float`s are encoded as IEEE 754 values with their octets in little-endian
    order.
  - `Double`s are encoded as IEEE 754 values with their octets in
    little-endian order.
  - `ByteString`s are encoded with their length prefixed according to the
    `Int` format.
  - `Text` values are encoded using a zero-terminated, modified UTF-8 format
    that works like UTF-8 except that it encodes `'\x0'` as `[0xC0,0x80]`.
    This format never outputs a '0x00' for any Unicode codepoint and can
    therefore be zero-terminated, which allows an efficient streaming
    encoding.  


The compact format
------------------

This is the default format. It trades some performance for compactness and
portability. `Int`s and `Word`s wider than 2 bytes are encoded using a
variable length base-128 encoding, as used by (Google's protocol bufffers)[https://developers.google.com/protocol-buffers/docs/encoding].


The throughput format
---------------------

This format is optmized for maximum throughput on 64bit, x86 machines. I
assume they are the future server machines of choice. All primitive values are
therefore encoded using a little-endian encoding.


The tagged format
-----------------

Before every primitive value a tag-byte is written indicating the type of the
following primitive value. This allows decoding a binary value to a
human-readable stream of primitive values.


API
---

In the first releases, all low-level encoding and decoding support is kept
internal. This simplifies experimentation. There is one abstract type for
`Encoder`s and one for `Decoder`s. The `Monoid` and `Monad` typeclasses are
provided as combintors for them.

    newtype Encoder a = ...
    newtype Decoder a = ...

    class Binary a where
      toBinary   :: Encoder a
      fromBinary :: Decoder a

Only one format is supported in the beginning. The untagged, throughput
format. This format gives a good baseline for the possible speed of the
implementation. We run an `Encoder` by converting it to a bytestring
`Builder`.

    encode :: Encoder a -> a -> Builder

We run a `Decoder` by converting it to an
(`Data.Attoparsec.ByteString.Result`)[http://hackage.haskell.org/packages/archive/attoparsec/0.10.1.1/doc/html/Data-Attoparsec-ByteString.html#t:Result].

    decode :: Decoder a -> Result a

Note that the decoder selects the appropriate format based on the 4-byte
prefix.

We provide convenience functions for the conversion to and from bytestrings.

    toBinaryBuilder          :: Binary a => a -> Builder
    toBinaryByteString       :: Binary a => a -> S.ByteString
    toBinaryLazyByteString   :: Binary a => a -> L.ByteString 
 
    fromBinaryByteString     :: Binary a => S.ByteString -> Either String a
    fromBinaryLazyByteString :: Binary a => L.ByteString -> Either String a



Security Concerns
=================

Note that the input of the decoder is *untrusted* and may be an arbitrary
sequence of bytes. The decoding implementation must make sure that for *any*
bytestring either an error is reported via `Left` or a Haskell value
satisfying *all* invariants of its type is returned. This entails for example
that we must validate every `Text` value. This also excludes using functions
such as `fromAscList` without having validated their input first.

The benefit of implementing fully validating decoders is that we can use them
for implementing public interfaces. If the cost of validation is too high then
we can consider implementing a second `UnsafeBinary` typeclass whose decoder
is only guaranteed to be correct for bytestrings in the range of the encoder.

Note that we must also take care to provide good bounds on the resource usage
of our implementation. This concerns heap space and stack space. Some
implementations require considerable stack space. They might profit from
catching `StackOverflow` exceptions and report them politely to their caller
using a `Left` result.

Note also that we must report overflows when decoding `Int` and `Word` values,
as we cannot guarantee that using a truncated 64-bit number will work.


TODO for a first release
========================

Implement the above API for the throughput format and benchmark against
binary, cereal, and attoparsec to catch regressions.


Future Work
===========

- Implement generic serialization (DONE by Bas van Dijk, needs benchmarking)
- Implement all four suggested formats
- Implement debugging decoder for tagged formats
- Implement error reporting for tagged format that produces human-readable
  output. It will still be flattened though.


