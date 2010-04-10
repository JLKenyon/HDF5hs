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
import HDF5hs.MidLevel

hdf5MainHello :: String
hdf5MainHello = "Hello from HDF5 (Macaw) - High Level"

data HDF5File = H5File      [HDF5Node]
                deriving (Show, Eq)

data HDF5Node = H5Group String [HDF5Node]
              | H5DataSet String HDF5Data
                deriving (Show, Eq)

data HDF5Data = H5IntData   [Int] [Int]
              | H5LongData  [Int] [Int]
              | H5ShortData [Int] [Int]
              | H5CharData  [Int] [Char]
              | H5FloatData [Int] [Float]
                deriving (Show, Eq)

writeHDF5File :: String -> HDF5File -> IO ()
writeHDF5File = undefined

loadHDF5File :: String -> IO HDF5File
loadHDF5File = undefined


--loadHDF5File :: String -> IO HDF5Obj
--loadHDF5File fname = do
--  withReadonlyHDF5File fname $ \handle -> do
--                       return $ File fname
                 









