{--
  
  Copyright (c) 2010, John Kenyon
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  
  * Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
  * Neither the name of John Kenyon nor the names of his contributors
  may be used to endorse or promote products derived from this software
  without specific prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
  
  --}


module TestLowLevel where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack,unpack)

import System.Timeout (timeout)

import Directory
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils (with)

import HDF5hs.LowLevel
import HDF5hs.MidLevel

import Data.Maybe (isJust)

lowLevelTestGroup = testGroup "Low level Interface Tests" 
  [ testCase "H5Fcreate" testH5Fcreate 
  , testCase "H5FCreate and Reopen" testH5FcreateAndReopen
  , testCase "H5FCreate and Check Size" testH5FcreateAndCheckSize
  ]

testH5Fcreate ::Assertion
testH5Fcreate = useAsCString (pack fn) $ \cfn -> do 
                  handle <- c_H5Fcreate cfn h5Foverwrite h5Fdefault h5Fdefault
                  c_H5Fclose handle
                  success <- doesFileExist fn
                  removeFile fn
                  assertBool "H5Fcreate failed to create a new HDF5 file"
                                 (unH5Handle handle >= 0)
                      where
                        fn = "/tmp/testH5FCreate.h5"

testH5FcreateWithInt :: Assertion
testH5FcreateWithInt =  useAsCString (pack fn) $ \ptr          -> do 
  useAsCString (pack "/data")                  $ \dpath        -> do
  withArray (toCInt [length newData])          $ \lenData      -> do
  withArray (toCInt newData)                   $ \bufferedData -> do
  handle <- c_H5Fcreate ptr h5Foverwrite h5Fdefault h5Fdefault 
  c_H5LTmake_dataset_int handle dpath (toEnum 1) lenData bufferedData
  c_H5Fclose handle
  success <- doesFileExist fn 
  removeFile fn
  assertBool "H5Fcreate failed to create a new HDF5 file" success
    where
      fn = "/tmp/testH5FCreateWithInt.h5"
      newData = [1..6]

testH5FcreateAndReopen :: Assertion
testH5FcreateAndReopen = useAsCString (pack fn) $ \cfn -> do 
  useAsCString (pack "/data") $ \dpath -> do
  withArray (toCInt [length newData]) $ \lenData -> do
  withArray (toCInt newData)          $ \bufData -> do
    handle <- c_H5Fcreate cfn h5Foverwrite h5Fdefault h5Fdefault 
    c_H5LTmake_dataset_int handle dpath (toEnum 1) lenData bufData
    c_H5Fclose handle
    handle2 <- c_H5Fopen cfn h5Freadonly h5Fdefault
    c_H5Fclose handle2
    removeFile fn
    assertBool "H5Fcreate failed to create and open a new HDF5 file" 
        ((unH5Handle handle2) >= 0)
    where
      fn = "/tmp/testH5FCreateAndReadWithInt.h5"
      newData = [1,2,3,4,5,6]


testH5FcreateAndCheckSize :: Assertion
testH5FcreateAndCheckSize = do 
  createTestFile testFile testData
  checkTestFile  testFile testData
    where
      testFile = "/tmp/testH5FCreateAndCheck.h5"
      testData = [1..6]
-- ---------------------------------------
      createTestFile :: String -> [Int] -> IO ()
      createTestFile fn newData = do
        useAsCString (pack fn)      $ \cfn -> do 
        useAsCString (pack "/data") $ \dpath -> do
        withArray (toCInt [length newData]) $ \lenData -> do
        withArray (toCInt newData)          $ \bufData -> do
          handle <- c_H5Fcreate cfn h5Foverwrite h5Fdefault h5Fdefault 
          c_H5LTmake_dataset_int handle dpath (toEnum 1) lenData bufData
          c_H5Fclose handle
          return ()
-- --------------------------------------
      checkTestFile :: String -> [Int] -> IO ()
      checkTestFile fn oldData = do 
        useAsCString (pack fn) $ \cfn -> do
        useAsCString (pack "/data") $ \dPath -> do
          handle <- c_H5Fopen cfn h5Freadonly h5Fdefault
          ndims <- getDatasetNdims handle "/data"
          c_H5Fclose handle
          assertBool "oh geez" (ndims == 1)



