{--
  
  Copyright (c) 2010 John Lincoln Kenyon
  
  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use,
  copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following
  conditions:
  
  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.
  
  --}

module HDF5hs.MidLevel.Util where

import HDF5hs.LowLevel.H5Types

import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack)

import HDF5hs.LowLevel (withCString)
import HDF5hs.LowLevel.H5F
import HDF5hs.LowLevel.H5LT
import HDF5hs.LowLevel.H5G
import HDF5hs.LowLevel.H5L
import HDF5hs.LowLevel.H5A
import HDF5hs.LowLevel.H5D
import HDF5hs.LowLevel.H5S
import HDF5hs.LowLevel.H5T

import Foreign.Ptr
import Foreign.C.Types (CChar, CInt,CULLong)
import Foreign.C.String (CString,peekCAString)
import Foreign.Marshal.Array (withArray,peekArray)
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Unsafe.Coerce (unsafeCoerce)


hdf5HelloMid = "Hello World Mid Level"

toCInt :: [Int] -> [CInt]
toCInt = map toEnum
--toCInt lst = map (\v -> (toEnum v)::CInt) lst

toC :: (Storable a, Enum a) => [Int] -> [a]
toC  = map toEnum

toInt :: [CInt] -> [Int]
toInt = map fromEnum
--toInt lst = map (\v -> (unsafeCoerce v)::Int) lst

fromC :: (Storable a, Enum a) => [a] -> [Int]
fromC  = map fromEnum

applyWithString :: (CString -> IO a) -> (a -> IO b) -> String -> (a -> IO c) -> IO c
applyWithString open_action close_action str func = do
  withCString str $ \cstr -> do
    handle <- open_action cstr
    val <- func handle
    close_action handle
    return val

getIntArrayByPtr :: Int -> (Ptr CInt -> IO ()) -> IO [Int]
getIntArrayByPtr size func = do
  withArray (toCInt [1..size]) $ \cptr -> do
    func cptr
    val <- peekArray size cptr
    return $ toInt val
 
getULLongArrayByPtr :: Int -> (Ptr CULLong -> IO ()) -> IO [Int]
getULLongArrayByPtr size func = do
  withArray (toC [1..size]) $ \cptr -> do
    func cptr
    val <- peekArray size cptr
    return $ fromC val

getStringByPtr :: Int -> (CString -> IO ()) -> IO String
getStringByPtr size func = do
  useAsCString (pack $ replicate size '\0') $ \cptr -> do
  func cptr
  peekCAString cptr

getULLongValByPtr :: (Ptr CULLong -> IO a) -> IO CULLong
getULLongValByPtr func = do
  alloca $ \cptr -> do
    func cptr
    ret <- peek cptr
    return ret
