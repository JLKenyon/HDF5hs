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

import Foreign.C.Types (CInt)
import Foreign.C.String (CString)
import Foreign.Marshal.Array (withArray,peekArray)

import Unsafe.Coerce (unsafeCoerce)

hdf5HelloMid = "Hello World Mid Level"

toCInt :: [Int] -> [CInt]
toCInt lst = map (\v -> (toEnum v)::CInt) lst

toInt :: [CInt] -> [Int]
toInt lst = map (\v -> (unsafeCoerce v)::Int) lst

withNewHDF5File :: String -> (H5Handle -> IO a) -> IO a
withNewHDF5File str func = do
  withCString str $ \cstr -> do
  handle <- c_H5Fcreate cstr h5Foverwrite h5Fdefault h5Fdefault
  val <- func handle
  c_H5Fclose handle
  return val

withHDF5File :: String -> (H5Handle -> IO a) -> IO a
withHDF5File str func = do
  withCString str $ \cstr -> do
  handle <- c_H5Fopen cstr h5Freadwrite h5Fdefault
  val <- func handle
  c_H5Fclose handle
  return val

getDatasetNdims :: H5Handle -> String -> IO Int
getDatasetNdims handle path = do
  withCString path $ \dPath -> do
  withArray [-128] $ \ptr   -> do -- 128 is giberish
    c_H5LTget_dataset_ndims handle dPath ptr
    val <- peekArray 1 ptr
    return $ unsafeCoerce $ head $ val

data H5DatasetInfo = H5DatasetInfo [Int] H5TypeClass Int
                     deriving (Show, Eq)

getDatasetInfo :: H5Handle -> String -> IO H5DatasetInfo
getDatasetInfo handle dPath = do
  withCString dPath $ \cdPath -> do
    ndims <- getDatasetNdims handle dPath
    datSize <- do
      withArray (toCInt [1..ndims]) $ \dimPtr     -> do
      withArray [h5Fno_class]       $ \classIdPtr -> do 
      withArray [0]                 $ \sizePtr    -> do
        c_H5LTget_dataset_info handle cdPath dimPtr classIdPtr sizePtr
        dimSize <- peekArray (unsafeCoerce ndims) dimPtr
        classId <- peekArray 1 classIdPtr
        size    <- peekArray 1 sizePtr
        return $ H5DatasetInfo (toInt dimSize) (head classId) (head $ toInt size)
    return $ datSize


putDatasetInt1D :: H5Handle -> String -> [Int] -> IO CInt
putDatasetInt1D handle dPath nData = do
  withCString dPath         $ \cdPath    -> do 
  withArray inputArrayDims  $ \lenBuffer -> do
  withArray (toCInt nData)  $ \datBuffer -> do
  ret <- c_H5LTmake_dataset_int handle cdPath (toEnum 1) lenBuffer datBuffer
  return ret
    where
      inputArrayDims = toCInt [length nData]

putDatasetInt2D :: H5Handle -> String -> [[Int]] -> IO CInt
putDatasetInt2D handle dPath nData = do
  withCString dPath        $ \cdPath    -> do
  withArray inputArrayDims $ \lenBuffer -> do
  withArray flatData       $ \datBuffer -> do
  ret <- c_H5LTmake_dataset_int handle cdPath (toEnum 2) lenBuffer datBuffer
  return ret
    where 
      inputArrayDims = toCInt ([length nData] ++ [ length (nData!!1)])
      flatData = toCInt (foldl (++) [] nData)

getDatasetInt1D :: H5Handle -> String -> IO (Maybe [Int])
getDatasetInt1D handle dPath = do
  withCString dPath $ \cdPath -> do
  info <- getDatasetInfo handle dPath
  if (False)
     then return $ Nothing
     else getDatasetInt1D' handle cdPath info
    where 
      verifyDims :: H5DatasetInfo -> Bool
      verifyDims (H5DatasetInfo dimSize _ _) = if (length dimSize) == 1
                                               then True
                                               else False
      getDatasetInt1D' :: H5Handle -> CString -> H5DatasetInfo -> IO (Maybe [Int])
      getDatasetInt1D' handle cdPath (H5DatasetInfo dimSize classType totalSize) = do
        withArray [1..(toEnum totalSize)] $ \datPtr -> do
        c_H5LTread_dataset_int handle cdPath datPtr
        dat <- peekArray (unsafeCoerce $ head dimSize) datPtr
        return $ Just (toInt dat)

withReadonlyHDF5File :: String -> (H5Handle -> IO a) -> IO a
withReadonlyHDF5File str func = do
  withCString str $ \cstr -> do
  handle <- c_H5Fopen cstr h5Freadonly h5Fdefault
  val <- func handle
  c_H5Fclose handle
  return val

createGroup :: H5Handle -> String -> IO H5Handle
createGroup handle label = do
  withCString label $ \clabel -> do
    c_H5Gcreate handle clabel 0

--c_H5Dcreate :: CInt -> CString -> CInt -> CInt -> CInt -> IO CInt
{- hid_t H5Dcreate( hid_t loc_id, 
                    const char *name, 
                    hid_t type_id, 
                    hid_t space_id, 
                    hid_t dcpl_id   ) -} 
createDataSet :: H5Handle -> String -> Int -> Int -> Int -> IO H5Handle
createDataSet handle str v1 v2 v3 = do
    withCString str $ \cstr ->
        c_H5Dcreate handle cstr cv1 cv2 cv3
    where cv1 = (toEnum v1)::CInt 
          cv2 = (toEnum v2)::CInt 
          cv3 = (toEnum v3)::CInt 





