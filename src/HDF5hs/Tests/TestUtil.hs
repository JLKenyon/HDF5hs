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

module TestUtil (withTempFileName, withTestDataOneD) where

import System.Posix.Temp (mkstemp)
import System.Directory (getTemporaryDirectory)
import System.IO (hClose)
import Directory
import System.FilePath.Posix ( (</>) )

import System.Random

import Control.Monad

import Foreign.C.Types (CInt)
import Foreign.C.String (CString)
import Foreign.Marshal.Array (withArray,peekArray)

toCInt :: [Int] -> [CInt]
toCInt lst = map (\v -> (toEnum v)::CInt) lst

getTestFileName :: IO String
getTestFileName = do
  tmpdir <- getTemporaryDirectory
  (fname,handle) <- mkstemp (tmpdir </> "TestHDF.h5.XXXXXX")
  hClose handle
  return fname

withTempFileName :: (String -> IO ()) -> IO ()
withTempFileName func = do
  fname <- getTestFileName
  func fname
  removeFile fname
  
getTestDataOneD :: (Int, Int) -> (Int, Int) -> IO [Int]
getTestDataOneD length_range value_range = do
  testLength <- randomRIO length_range
  mapM (\x -> randomRIO value_range) [1..testLength]

withTestDataOneD :: (Int, Int) -> (Int, Int) -> ([Int] -> IO b) -> IO b
withTestDataOneD len_rng val_rng func = do
  testData <- getTestDataOneD len_rng val_rng
  func testData

