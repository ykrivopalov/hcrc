module Main where

import Data.Digest.CRC

import Control.Monad
import Data.Word
import System.Exit
import Test.HUnit.Base
import Test.HUnit.Text

table :: LookupTable Word16
table = makeTableBE 0x1021

tests :: Test
tests = "Basic" ~: [
             (0x29B1 :: Word16) ~=? crcBE 0x1021 0xffff simple_string
            ,(0x31C3 :: Word16) ~=? crcBE 0x1021 0x0000 simple_string
            ,(0xA99F0B9E :: Word32) ~=? crcBE 0x10211021 0xFFFFFFFF simple_string
            ,(0xB89E592C18B78C0 :: Word64) ~=? crcBE 0x102110211021 0xFFFFFFFFFFFF simple_string
            ,(0x29B1 :: Word16) ~=? crcTableBE table 0xffff simple_string
            ,(0x31C3 :: Word16) ~=? crcTableBE table 0x0000 simple_string
        ]
        where
            simple_string = [fromIntegral (fromEnum x) :: Word8 | x <- "123456789"]

testFails counts = errors counts /= 0 || failures counts /= 0

main = do result <- runTestTT tests
          when (testFails result) exitFailure

