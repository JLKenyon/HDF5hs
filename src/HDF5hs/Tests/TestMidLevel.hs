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
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
  --}

module TestMidLevel (midLevelTestGroup) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack)

import Directory
import Foreign.Marshal.Array

import HDF5hs.LowLevel
import HDF5hs.MidLevel

midLevelTestGroup = testGroup "Mid level Interface Tests" 
                    [ testCase "H5FcreateMid"        $ testH5FcreateMid
                    , testCase "withHDF5File"        $ testWithHDF5File
                    , testCase "putDatasetInt"       $ testPutDatasetInt
                    , testCase "putAndGetDatasetInt" $ testPutAndGetDatasetInt
                    , testCase "putDatasetIntOneDim" $ testPutDataIntOneDim
                    ]

testH5FcreateMid :: Assertion
testH5FcreateMid = do
  withNewHDF5File fn $ \handle -> do return ()
  ret <- doesFileExist fn
  removeFile fn
  assertBool "withNewHDF5File failed to create a new file" ret
    where fn = "/tmp/testMidCreate.h5"

testPutDatasetInt :: Assertion
testPutDatasetInt =  do
  withNewHDF5File fn $ \handle -> putDatasetInt1D handle dpath [1..8]
  ret <- doesFileExist fn
  removeFile fn
  assertBool "testPuDatasetInt failed to create a new file" True
    where fn    = "/tmp/testPutDatesetInt1D.h5"
          dpath = "/testdata"

testWithHDF5File :: Assertion
testWithHDF5File = do
  withNewHDF5File fn $ \handle -> do return ()
  ret <- withHDF5File fn $ \handle -> do return ((unH5Handle handle) >= 0)
  removeFile fn
  assertBool "Could not reopen an HDF5 file" ret
    where fn = "/tmp/testWithHDF5File.h5"

testPutAndGetDatasetInt :: Assertion
testPutAndGetDatasetInt =  do
  withNewHDF5File     fn $ \handle -> do putDatasetInt1D handle dpath testData
  val <- withHDF5File fn $ \handle -> do getDatasetInt1D handle dpath
--  removeFile fn
  assertBool "testPuDatasetInt failed to create a new file" 
             (val == Just testData)
    where fn       = "/tmp/testPutAndGetDatesetInt1D.h5"
          dpath    = "/testdata"
          testData = [1..8]

testPutDataIntOneDim :: Assertion
testPutDataIntOneDim = do 
  useAsCString (pack fn)      $ \cfn       -> do 
  useAsCString (pack dPath)   $ \cdPath    -> do 
  withArray    (toCInt lDat)  $ \lenBuffer -> do 
  withArray    (toCInt nData) $ \datBuffer -> do
     handle  <- c_H5Fcreate cfn h5Foverwrite h5Fdefault h5Fdefault
     lbufval <- peekArray   1   lenBuffer
     st <- c_H5LTmake_dataset_int handle cdPath (toEnum 1) lenBuffer datBuffer
     c_H5Fclose handle
     removeFile fn
     assertBool "testPutDatasetIntOneDim returned fail" (st <= 0)
         where
           fn    = "/tmp/testPutDataIntOneDim.h5"
           dPath = "/mydata"
           nData = [1..6]
           lDat  = [toEnum $ length nData]

