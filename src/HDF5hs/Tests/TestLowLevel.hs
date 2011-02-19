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
import Foreign.C.Types (CInt)

import HDF5hs.LowLevel
import HDF5hs.LowLevel.H5F 
import HDF5hs.LowLevel.H5A 
import HDF5hs.LowLevel.H5L 
import HDF5hs.LowLevel.H5G 
import HDF5hs.LowLevel.H5D 
import HDF5hs.LowLevel.H5LT 
import HDF5hs.LowLevel.H5Types 

import HDF5hs.MidLevel.Util (toCInt)

import System.Random

import Data.Maybe (isJust)
import Unsafe.Coerce (unsafeCoerce)
import TestUtil (withTempFileName, withTestDataOneD)

lowLevelTestGroup = testGroup "Low level Interface Tests"
  [ testCase "H5Fcreate" testH5Fcreate 
  , testCase "H5FCreate and Reopen" testH5FcreateAndReopen
  , testCase "H5FCreate and Check Dims" testH5FcreateAndCheckNumDims
  , testCase "H5FCreate and Check Size" testH5FcreateAndCheckSize
  , testCase "H5FCreate and Check" testH5FcreateAndCheck
  ]
 
testH5Fcreate ::Assertion
testH5Fcreate = do
  withTempFileName   $ \fn        -> do
    useAsCString (pack fn) $ \cfn -> do 
      handle <- c_H5Fcreate cfn h5F_overwrite h5F_default h5F_default
      c_H5Fclose handle
      success <- doesFileExist fn
      assertBool "H5Fcreate failed to create a new HDF5 file"
                 ((unH5Handle handle) >= 0 && success)
 
testH5FcreateWithInt :: Assertion
testH5FcreateWithInt =  do
  withTempFileName   $ \fn     -> do
    useAsCString (pack fn)              $ \ptr          -> do 
    useAsCString (pack "/data")         $ \dpath        -> do
    withArray (toCInt [length newData]) $ \lenData      -> do
    withArray (toCInt newData)          $ \bufferedData -> do
    handle <- c_H5Fcreate ptr h5F_overwrite h5F_default h5F_default 
    c_H5LTmake_dataset_int handle dpath (toEnum 1) lenData bufferedData
    c_H5Fclose handle
    success <- doesFileExist fn 
    assertBool "H5Fcreate failed to create a new HDF5 file" success
      where
        newData = [2..6]
 
testH5FcreateAndReopen :: Assertion
testH5FcreateAndReopen = do
  withTempFileName   $ \fn     -> do
    useAsCString (pack fn) $ \cfn -> do 
    useAsCString (pack "/data") $ \dpath -> do
    withArray (toCInt [length newData]) $ \lenData -> do
    withArray (toCInt newData)          $ \bufData -> do
      handle <- c_H5Fcreate cfn h5F_overwrite h5F_default h5F_default 
      c_H5LTmake_dataset_int handle dpath (toEnum 1) lenData bufData
      c_H5Fclose handle
      handle2 <- c_H5Fopen cfn h5F_readonly h5F_default
      c_H5Fclose handle2
      assertBool "H5Fcreate failed to create and open a new HDF5 file" 
          ((unH5Handle handle2) >= 0)
      where
        newData = [2..6]
 
 
testH5FcreateAndCheckNumDims :: Assertion
testH5FcreateAndCheckNumDims = do   
  withTempFileName   $ \fn     -> do
  createTestFile fn testData
  checkTestFile  fn testData
    where
      testData = [2..6]
-- ---------------------------------------
      createTestFile :: String -> [Int] -> IO ()
      createTestFile fn newData = do
        useAsCString (pack fn)      $ \cfn -> do 
        useAsCString (pack "/data") $ \dpath -> do
        withArray (toCInt [length newData]) $ \lenData -> do
        withArray (toCInt newData)          $ \bufData -> do
          handle <- c_H5Fcreate cfn h5F_overwrite h5F_default h5F_default 
          c_H5LTmake_dataset_int handle dpath (toEnum 1) lenData bufData
          c_H5Fclose handle
          return ()
-- --------------------------------------
      checkTestFile :: String -> [Int] -> IO ()
      checkTestFile fn oldData = do 
        useAsCString (pack fn) $ \cfn -> do
        useAsCString (pack "/data") $ \dPath -> do
          handle <- c_H5Fopen cfn h5F_readonly h5F_default
          ndims <- getDatasetNdims handle "/data"
          c_H5Fclose handle
          assertBool "oh geez" (ndims == 1)
-- --------------------------------------
      getDatasetNdims :: H5Handle -> String -> IO Int
      getDatasetNdims handle path = do
        withCString path $ \dPath -> do
        withArray [-128] $ \ptr   -> do -- 128 is giberish
          c_H5LTget_dataset_ndims handle dPath ptr
          val <- peekArray 1 ptr
          return $ unsafeCoerce $ head $ val

        
 
testH5FcreateAndCheckSize :: Assertion
testH5FcreateAndCheckSize = do 
  withTempFileName   $ \fn     -> do
  createTestFile fn testData
  checkTestFile  fn testData
    where
      testPath = "/data"
      testData = [2..6]
-- ---------------------------------------
      createTestFile :: String -> [Int] -> IO ()
      createTestFile fn newData = do
        useAsCString (pack fn)      $ \cfn -> do 
        useAsCString (pack testPath) $ \dpath -> do
        withArray (toCInt [length newData]) $ \lenData -> do
        withArray (toCInt newData)          $ \bufData -> do
          handle <- c_H5Fcreate cfn h5F_overwrite h5F_default h5F_default 
          c_H5LTmake_dataset_int handle dpath (toEnum 1) lenData bufData
          c_H5Fclose handle
          return ()
-- --------------------------------------
      checkTestFile :: String -> [Int] -> IO ()
      checkTestFile fn oldData = do 
        useAsCString (pack fn) $ \cfn -> do
        useAsCString (pack testPath) $ \cdPath -> do
          handle <- c_H5Fopen cfn h5F_readonly h5F_default
          ndims <- withArray [0] $ \ptr -> do
            c_H5LTget_dataset_ndims handle cdPath ptr
            ndimsArray <- peekArray 1 ptr
            return $ unsafeCoerce $ head $ ndimsArray
          datSize <- do
            withArray [1..ndims]     $ \dimPtr     -> do
            withArray [h5F_no_class] $ \classIdPtr -> do 
            withArray [0]            $ \sizePtr    -> do
              c_H5LTget_dataset_info handle  cdPath dimPtr classIdPtr sizePtr
              peekArray (unsafeCoerce ndims) dimPtr
          assertBool "Could not verify data size" 
                         ((map unsafeCoerce datSize) == [length testData])
 
testH5FcreateAndCheck :: Assertion
testH5FcreateAndCheck = do 
  withTestDataOneD  (20,40) (-1000,1000) $ \testDataVal -> do
  withTempFileName   $ \fn     -> do
  createTestFile fn testDataVal
  checkTestFile  fn testDataVal
    where
      testFile = "/tmp/testH5FCreateAndCheck.h5"
      testPath = "/data"
      --testData = [2..7]
-- ---------------------------------------
      createTestFile :: String -> [Int] -> IO ()
      createTestFile fn newData = do
        useAsCString (pack fn)      $ \cfn -> do 
        useAsCString (pack testPath) $ \dpath -> do
        withArray (toCInt [length newData]) $ \lenData -> do
        withArray (toCInt newData)          $ \bufData -> do
          handle <- c_H5Fcreate cfn h5F_overwrite h5F_default h5F_default 
          c_H5LTmake_dataset_int handle dpath (toEnum 1) lenData bufData
          c_H5Fclose handle
          return ()
-- --------------------------------------
      checkTestFile :: String -> [Int] -> IO ()
      checkTestFile fn oldData = do 
        useAsCString (pack fn) $ \cfn -> do
        useAsCString (pack testPath) $ \cdPath -> do
          handle <- c_H5Fopen cfn h5F_readonly h5F_default
          ndims <- withArray [0] $ \ptr -> do
            c_H5LTget_dataset_ndims handle cdPath ptr
            ndimsArray <- peekArray 1 ptr
            return $ unsafeCoerce $ head $ ndimsArray
          datSize <- do
            withArray [1..ndims]     $ \dimPtr     -> do
            withArray [h5F_no_class] $ \classIdPtr -> do 
            withArray [0]            $ \sizePtr    -> do
              c_H5LTget_dataset_info handle  cdPath dimPtr classIdPtr sizePtr
              cDat <- peekArray (unsafeCoerce ndims) dimPtr
              return $ head cDat
          dat <- do
            withArray [1..datSize] $ \datPtr -> do
            c_H5LTread_dataset_int handle cdPath datPtr
            peekArray (unsafeCoerce datSize) datPtr
          assertBool "Could not verify data" 
                         ((map unsafeCoerce dat) == oldData)
