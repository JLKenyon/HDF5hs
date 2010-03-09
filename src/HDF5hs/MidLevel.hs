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


module HDF5hs.MidLevel where

import System.Environment
import Data.List
import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack)

import HDF5hs.LowLevel
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable

hdf5HelloMid = "Hello World Mid Level"

toCInt :: [Int] -> [CInt]
toCInt lst = map (\v -> (toEnum v)::CInt) lst

withNewHDF5File :: String -> (H5Handle -> IO a) -> IO a
withNewHDF5File str func = useAsCString (pack str) $ \cstr -> do
  handle <- c_H5Fcreate cstr h5Foverwrite h5Fdefault h5Fdefault
  val <- func handle
  c_H5Fflush handle h5Sglobal
  c_H5Fclose handle
  return val

putDatasetInt1D :: H5Handle -> String -> [Int] -> IO CInt
putDatasetInt1D handle dPath nData = useAsCString (pack dPath) $ \cdPath -> do 
  withArray inputArrayDims                       $ \lenBuffer -> do
  withArray (map (\v -> (toEnum v)::CInt) nData) $ \datBuffer -> do
  ret <- c_H5LTmake_dataset_int handle cdPath (toEnum 1) lenBuffer datBuffer
  c_H5Fflush handle h5Slocal
  return ret
    where
      inputArrayDims = toCInt [length nData]

putDatasetInt2D :: H5Handle -> String -> [[Int]] -> IO CInt
putDatasetInt2D handle dPath nData = useAsCString (pack dPath) $ \cdPath -> do
  withArray inputArrayDims $ \lenBuffer -> do
  withArray flatData $ \datBuffer -> do
  ret <- c_H5LTmake_dataset_int handle cdPath (toEnum 2) lenBuffer datBuffer
  c_H5Fflush handle h5Slocal
  return ret
    where 
      inputArrayDims = toCInt ([length nData] ++ [ length (nData!!1)])
      flatData = toCInt (foldl (++) [] nData)

withReadonlyHDF5File :: String -> (H5Handle -> IO a) -> IO a
withReadonlyHDF5File str func = useAsCString (pack str) $ \cstr -> do
  handle <- c_H5Fopen cstr h5Freadonly h5Fdefault
  val <- func handle
  c_H5Fclose handle
  return val

getDatasetNdims :: H5Handle -> String -> IO [CInt]
getDatasetNdims handle path = do
  useAsCString (pack path) $ \dPath -> do
    ptr <- newArray [-128]
    c_H5LTget_dataset_ndims handle dPath ptr
    val <- peekArray 1 ptr
    free ptr
    return val
