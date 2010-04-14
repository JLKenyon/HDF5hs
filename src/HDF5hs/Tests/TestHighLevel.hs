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

module TestHighLevel (highLevelTestGroup) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack)

import TestUtil (withTempFileName)
import HDF5hs

highLevelTestGroup = testGroup "High level Interface Tests" 
  [ testCase "Compare Empty"            $ testEqEmpty
  , testCase "Compare Trivial"          $ testEqTrivial
  , testCase "Compare Complex One"      $ testEqOne
  , testCase "createAndLoad Empty"      $ testCreateAndLoadEmpty
  , testCase "createAndLoad Trivial"    $ testCreateAndLoadTrivial
  , testCase "CreateAndLoad Char"       $ testCreateAndLoadChars
  , testCase "CreateAndLoad Short"      $ testCreateAndLoadShorts
  , testCase "CreateAndLoad Int"        $ testCreateAndLoadInt
  , testCase "CreateAndLoad Int Two"    $ testCreateAndLoadIntTwo
  , testCase "CreateAndLoad Int Three"  $ testCreateAndLoadIntThree
  , testCase "CreateAndLoad Int Three"  $ testCreateAndLoadIntFour
  , testCase "CreateAndLoad Int Three"  $ testCreateAndLoadIntFive
  , testCase "CreateAndLoad MultiGroup" $ testCreateAndLoadMultiGroup
  ]

testCreateAndLoadTemplate :: HDF5File -> Assertion
testCreateAndLoadTemplate testData = do
  withTempFileName   $ \fn -> do
  writeHDF5File fn testData
  readData <- loadHDF5File fn
  assertBool "Error data mismatch between write and read" (testData == readData)

testEqTemplate :: HDF5File -> Assertion
testEqTemplate val = assertBool "" (val == val)

testEqEmpty   = testEqTemplate (H5File [])
testEqTrivial = testEqTemplate (H5File [H5Group "/" []])
testEqOne     = testEqTemplate (
                  H5File [H5Group "/" 
                          [
                           H5DataSet "structure" (
                             H5DataSpace [15,3] (H5IntData ldata ))]])
    where ldata = [ 0, 1, -1, 20, 2, 0, 26, 2, 1, 32, 2, 2, 38, 2, 2, 44, 2, 
                    1, 50, 2, 5, 56, 2, 5, 62, 3, 0, 68, 3, 8, 74, 3, 9,80, 
                    3, 9, 86, 3, 8, 92, 3, 12, 98, 3, 12 ]

testCreateAndLoadEmpty   = testCreateAndLoadTemplate (H5File [])
testCreateAndLoadTrivial = testCreateAndLoadTemplate 
                           (H5File [H5Group "/group" []])

testCreateAndLoadChars    = testCreateAndLoadTemplate 
                            (H5File [H5DataSet "myCharData" 
                                     (H5DataSpace [(length "Hello World")] 
                                      (H5CharData "Hello World")
                                     )])

testCreateAndLoadShorts    = testCreateAndLoadTemplate
                            (H5File [H5DataSet "myShortData"
                                     (H5DataSpace [1]
                                      (H5ShortData [12])
                                     )])

testCreateAndLoadInt     = testCreateAndLoadTemplate
                            (H5File [H5DataSet "myIntData"
                                     (H5DataSpace [1]
                                      (H5IntData [42])
                                     )])

testCreateAndLoadIntTwo  = testCreateAndLoadTemplate
                            (H5File [H5DataSet "myIntData"
                                     (H5DataSpace [4]
                                      (H5IntData [10,20,30,40])
                                     )])

testCreateAndLoadIntThree = testCreateAndLoadTemplate
                            (H5File [H5DataSet "myIntData"
                                     (H5DataSpace [4,4]
                                      (H5IntData [10,20,30,40,
                                                  11,21,31,41,
                                                  12,22,32,42,
                                                  13,23,33,43])
                                     )])

testCreateAndLoadIntFour  = testCreateAndLoadTemplate
                            (H5File [H5DataSet "myMaxByte"
                                     (H5DataSpace [4]
                                      (H5IntData [255,255,255,255])
                                     )])

testCreateAndLoadIntFive  = testCreateAndLoadTemplate
                            (H5File [H5DataSet "myMaxBytePlusOne"
                                     (H5DataSpace [4]
                                      (H5IntData [256,256,256,256])
                                     )])

testCreateAndLoadMultiGroup = testCreateAndLoadTemplate
                            (H5File [
                              H5Group "/group1" []
                             ,H5Group "/group2" []
                             ])                              