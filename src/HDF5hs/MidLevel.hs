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

withNewHDF5File :: String -> (H5Handle -> IO a) -> IO a
withNewHDF5File str func = do
  withCString str $ \cstr -> do
  handle <- c_H5Fcreate cstr h5F_overwrite h5F_default h5F_default
  val <- func handle
  c_H5Fclose handle
  return val

withHDF5File :: String -> (H5Handle -> IO a) -> IO a
withHDF5File str func = do
  withCString str $ \cstr -> do
  handle <- c_H5Fopen cstr h5F_readwrite h5F_default
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
      withArray [h5F_no_class]      $ \classIdPtr -> do 
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
  handle <- c_H5Fopen cstr h5F_readonly h5F_default
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


---- hid_t H5Screate_simple(int rank, const hsize_t * dims, const hsize_t * maxdims )
--foreign import ccall "hdf5.h H5Screate_simple"
--        c_H5Screate_simple :: CInt -> Ptr CULLong -> Ptr CULLong -> IO H5Handle

createDataSetSimple :: [Int] -> IO H5Handle
createDataSetSimple dims = do
  withArray ldims $ \cdims -> do
  c_H5Screate_simple crank cdims nullPtr
  where
    crank = (toEnum $ length dims)::CInt
    ldims :: [CULLong]
    ldims = (map toEnum dims)

  
createDataSet :: H5Handle -> String -> H5Handle -> H5Handle -> H5Handle -> IO H5Handle
createDataSet handle str v1 v2 v3 = do
    withCString str $ \cstr ->
        c_H5Dcreate handle cstr v1 v2 v3


withHDF5DataSpace :: [Int] -> (H5Handle -> IO b) -> IO b
withHDF5DataSpace dims func = do
  sHandle <- createDataSetSimple dims 
  ret <- func sHandle
  c_H5Sclose sHandle
  return ret

withHDF5DataTypeCopy :: H5Handle -> (H5Handle -> IO b) -> IO b
withHDF5DataTypeCopy handle func = do
  tHandle <- c_H5Tcopy handle
  ret <- func tHandle
  c_H5Tclose tHandle
  return ret

withHDF5DataSet :: H5Handle -> String -> H5Handle -> H5Handle -> (H5Handle -> IO b) -> IO b
withHDF5DataSet handle label htype hdataspace func = do
  useAsCString (pack label) $ \clabel -> do
  dhandle <- c_H5Dcreate handle clabel htype hdataspace h5P_default
  ret <- func dhandle
  c_H5Dclose dhandle
  return ret

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

getULLongValByPtr :: (Ptr CULLong -> IO a) -> IO Int
getULLongValByPtr func = do
  alloca $ \cptr -> do
    func cptr
    ret <- peek cptr
    return $ unsafeCoerce ret

getNumObjectsFromGroup :: H5Handle -> IO Int
getNumObjectsFromGroup handle = getULLongValByPtr (c_H5Gget_num_objs handle)

--c_H5Gget_objname_by_idx :: H5Handle -> CULLong -> CString -> CULLong -> IO CULLong

getObjectNameByIndex :: H5Handle -> Int -> IO String
getObjectNameByIndex handle idx = do
  getStringByPtr len wrap_get
    where
      wrap_get :: CString -> IO ()
      wrap_get cstr = do
        c_H5Gget_objname_by_idx handle (toEnum idx) cstr (toEnum len)
        return ()
      len = 1024

getGroup :: H5Handle -> String -> IO H5Handle
getGroup handle name = do
  withCString name $ \cname -> do
    c_H5Gopen handle cname

--getNumObjectsFromGroup handle = do
--  withArray (map toEnum [0]) $ \cptr -> do
--    c_H5Gget_num_objs handle cptr
--    val <- peek cptr
--    return $ unsafeCoerce val



