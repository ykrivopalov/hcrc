module Data.Digest.CRC ( LookupTable -- lookup table
                       -- network byte order as default
                       , crc -- evaluate crc for byte sequence
                       , makeTable -- create lookup table for given polynom
                       , crcTable -- evaluate crc using looku table
                       -- little endian
                       , crcLE
                       , makeTableLE
                       , crcTableLE
                       -- big endian
                       , crcBE
                       , makeTableBE
                       , crcTableBE ) where

import Data.Array
import Data.Bits
import Data.List
import Data.Word


byteSize = 8

crc ::  Bits a => a -> a -> [Word8] -> a
crc poly init bs = crcBE poly init bs
crcBE poly init bs = foldl' (crcByteBE poly) init bs
crcLE poly init bs = foldl' (crcByteLE poly) init bs

crcByteBE poly init b =
    iterate (updateBit poly) xored !! byteSize
    where xored = init `xor` shiftL (fromIntegral b) shiftOffset
          shiftOffset = (bitSize poly) - byteSize
          updateBit poly crc = if testBit crc (bitSize poly - 1)
                               then shiftL crc 1 `xor` poly
                               else shiftL crc 1

crcByteLE poly init b =
    iterate (updateBit poly) xored !! byteSize
    where xored = init `xor` (fromIntegral b)
          updateBit poly crc = if testBit crc 0
                               then shiftR crc 1 `xor` poly
                               else shiftR crc 1


type LookupTable a = Array Word8 a

generateTable poly makeCell =
    listArray (0, 255) [makeCell poly b | b <- [0..255]]

makeTable ::  Bits a => a -> LookupTable a
makeTable poly = makeTableBE poly

makeTableBE poly =
    generateTable poly makeCell
    where makeCell poly b =
              (crcByteBE poly 0 b) `xor` (shiftL (fromIntegral b) (bitSize poly))

makeTableLE poly =
    generateTable poly makeCell
    where makeCell poly b =
              (crcByteLE poly 0 b) `xor` (shiftR (fromIntegral b) byteSize)


crcTable :: (Integral a, Bits a) => LookupTable a -> a -> [Word8] -> a
crcTable table initial = crcTableBE table initial
crcTableBE table initial = foldl' (crcByteTableBE table) initial
crcTableLE table initial = foldl' (crcByteTableLE table) initial

crcByteTableBE table crc b =
    shiftL crc' byteSize `xor` (table ! idx)
    where idx = fromIntegral (shiftR crc' shiftOffset)
          crc' = crc `xor` (shiftL (fromIntegral b) shiftOffset)
          shiftOffset = bitSize crc - byteSize

crcByteTableLE table init b =
    shiftR crc' byteSize `xor` (table ! idx)
    where idx = fromIntegral (crc' .&. 0xFF)
          crc' = init `xor` fromIntegral b

