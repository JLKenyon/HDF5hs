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

module HDF5hs where

import HDF5hs.LowLevel
import HDF5hs.LowLevel.H5A     
import HDF5hs.LowLevel.H5D     
import HDF5hs.LowLevel.H5F     
import HDF5hs.LowLevel.H5G     
import HDF5hs.LowLevel.H5L     
import HDF5hs.LowLevel.H5LT    
import HDF5hs.LowLevel.H5Types 
import HDF5hs.LowLevel.H5S
import HDF5hs.LowLevel.H5T
import HDF5hs.LowLevel.H5D
import HDF5hs.MidLevel

import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack)
import Data.Char (ord)
import Foreign.C.Types (CInt)
import Foreign.C.String (CString)
import Foreign.Marshal.Array
import Foreign.Ptr (castPtr)


hdf5MainHello :: String
hdf5MainHello = "Hello from HDF5 - High Level"

data HDF5File = H5File      [HDF5Node]
                deriving (Show, Eq)

data HDF5Node = H5Group String [HDF5Node]
              | H5DataSet String HDF5DataSpace
                deriving (Show, Eq)

data HDF5DataSpace = H5DataSpace { dims :: [Int], dataSet :: HDF5Data }
                     deriving (Show, Eq)

data HDF5Data = H5IntData   [Int]
              | H5LongData  [Int]
              | H5ShortData [Int]
              | H5CharData  [Char]
              | H5FloatData [Float]
                deriving (Show, Eq)

writeHDF5File :: String -> HDF5File -> IO ()
writeHDF5File fname (H5File groups) = do
  withNewHDF5File fname $ \handle -> do
    mapM (writeHDF5Group handle) groups
    return ()

writeHDF5Group :: H5Handle -> HDF5Node -> IO ()
writeHDF5Group handle (H5Group label groups) = do
  ghandle <- createGroup handle label 
  mapM (writeHDF5Group ghandle) groups
  return ()

writeHDF5Group handle (H5DataSet label dataSpace) = do
  withHDF5DataSpace (dims dataSpace)  $ \sHandle -> do
  withHDF5DataTypeCopy h5T_native_int $ \tHandle -> do
  c_H5Tset_order tHandle h5T_order_le
  withHDF5DataSet handle label tHandle sHandle $ \dsHandle -> do
    writeHDF5Data dsHandle (dataSet dataSpace)
    return ()

writeHDF5Data :: H5Handle -> HDF5Data -> IO CInt
writeHDF5Data handle (H5IntData ldat) = do
  withArray (map toEnum ldat) $ \cdat -> do
    c_H5Dwrite handle h5T_native_int h5S_all h5S_all h5P_default cdat

writeHDF5Data handle (H5LongData ldat) = do
  withArray (map toEnum ldat) $ \cdat -> do
    c_H5Dwrite handle h5T_native_long h5S_all h5S_all h5P_default cdat

writeHDF5Data handle (H5ShortData ldat) = do
  withArray (map toEnum ldat) $ \cdat -> do
    c_H5Dwrite handle h5T_native_short h5S_all h5S_all h5P_default cdat

writeHDF5Data handle (H5CharData ldat) = do
  withArray (map (toEnum . ord) ldat) $ \cdat -> do
    c_H5Dwrite handle h5T_native_char h5S_all h5S_all h5P_default cdat

--writeHDF5Data handle (H5FloatData ldat) = do
--  withArray ldat $ \cdat -> do
--    c_H5Dwrite handle h5T_native_float h5S_all h5S_all h5P_default cdat

-- ------------------------------------------

loadHDF5File :: String -> IO HDF5File
loadHDF5File fname = do
  withReadonlyHDF5File fname $ \handle -> do
    groups <- loadHDF5Groups handle
    return $ H5File groups

loadHDF5Groups :: H5Handle -> IO [HDF5Node]
loadHDF5Groups handle = do
  num_groups <- getNumObjectsFromGroup handle
  names <- mapM (getObjectNameByIndex handle) [0..(num_groups-1)]
--  group_handle <- mapM (c_H5Gopen handle) names
--  putStrLn $ show group_handle
--c_H5Gopen
--putStrLn $ show names  
  undefined


