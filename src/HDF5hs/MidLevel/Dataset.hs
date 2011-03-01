{--
  
  Copyright (c) 2011 John Lincoln Kenyon
  
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

module HDF5hs.MidLevel.Dataset where

-- Reference junk
---- TODO - CULLong should be replaced with something
----        in H5Types
--import Foreign.C.Types (CULLong)
-- 
--import HDF5hs.LowLevel.H5Types
--import HDF5hs.LowLevel.H5G
--import HDF5hs.MidLevel.Util (applyWithString, getULLongValByPtr)
-- 
------withNewGroup  :: H5Handle -> String -> (H5Handle -> IO a) -> IO a
-- 
----newDataset :: H5Handle -> String -> H5Handle -> H5Handle -> H5Handle -> IO H5Handle
---- 
----withNewDataset :: H5Handle -> String -> H5Handle -> H5Handle -> H5Handle
---- 
----withDataset

