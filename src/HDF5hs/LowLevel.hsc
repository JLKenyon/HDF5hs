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

{-# LANGUAGE CPP, ForeignFunctionInterface #-}



module HDF5hs.LowLevel
{--    ( hdf5hello -- This was a silly idea...
    , h5Foverwrite
    , h5Fabort
    , h5Fdefault
    , c_H5Fopen
    , c_H5Fcreate
    , c_H5Fflush
    , c_H5LTmake_dataset_int
    )--}
    where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Marshal.Array

import Control.Monad

import Foreign.Storable as Store
import Foreign.Storable (Storable (..), )

import Data.Int
import Data.Word
import Foreign.StablePtr


#include <hdf5.h>

-- | The hdf5hello is a test function used to verify that the
--   library has loaded correctly. Try main = print hdf5hello
hdf5hello :: String
hdf5hello = "Hello from HDF5 - LowLevel"

newtype H5OpenFlag = H5OpenFlag { unH5OpenFlag  :: CInt }
    deriving (Eq,Show)

#{enum H5OpenFlag, H5OpenFlag
 , h5Foverwrite = H5F_ACC_TRUNC
 , h5Fabort     = H5F_ACC_EXCL
 }

newtype H5OpenMode = H5OpenMode { unH5OpenMode  :: CInt }
    deriving (Eq,Show)

#{enum H5OpenMode, H5OpenMode
 , h5Freadonly  = H5F_ACC_RDONLY
 , h5Freadwrite = H5F_ACC_RDWR
 }

newtype H5TypeClass = H5TypeClass { unH5TypeClass :: CInt }
    deriving (Eq, Show, Storable)

-- This doesn't seem to work!  I lifted the code from Hugs (not GHC)
-- I suppose I should report this or something?
--instance Storable (H5TypeClass) where
--  sizeOf      _     = sizeOf (undefined::Int32)
--  alignment   _     = alignment (undefined::Int32)
----  peekElemOff p i   = liftM (/= (0::Int32)) & peekElemOff (castPtr p) i
----  pokeElemOff p i x = pokeElemOff (castPtr p) i x

#{enum H5TypeClass, H5TypeClass
 , h5Fno_class  = H5T_NO_CLASS 
 , h5Finteger   = H5T_INTEGER  
 , h5Ffloat     = H5T_FLOAT    
 , h5Ftime      = H5T_TIME     
 , h5Fstring    = H5T_STRING   
 , h5Fbitfield  = H5T_BITFIELD 
 , h5Fopaque    = H5T_OPAQUE   
 , h5Fcompound  = H5T_COMPOUND 
 , h5Freference = H5T_REFERENCE
 , h5Fenum      = H5T_ENUM     
 , h5Fvlen      = H5T_VLEN     
 , h5Farray     = H5T_ARRAY    
 , h5Fnclasses  = H5T_NCLASSES 
 }
 
h5Fdefault :: CInt
h5Fdefault = #const H5P_DEFAULT

newtype H5Scope = H5Scope { unH5Scope :: CInt }
    deriving (Eq, Show)

#{enum H5Scope, H5Scope
 , h5Slocal  = H5F_SCOPE_LOCAL
 , h5Sglobal = H5F_SCOPE_GLOBAL
 , h5Sdown   = H5F_SCOPE_DOWN
}

newtype H5Handle = H5Handle {unH5Handle :: CInt }
    deriving (Eq,Show)

-- ---------------------------------------------------------------------------------------
--hid_t  H5Fcreate               ( const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id ) 
foreign import ccall "hdf5.h H5Fcreate"
        c_H5Fcreate :: CString -> H5OpenFlag -> CInt -> CInt -> IO H5Handle

-- ---------------------------------------------------------------------------------------
--hid_t  H5Fopen                 ( const char *name, unsigned flags, hid_t fapl_id ) 
-- flags   = H5F_ACC_TRUNC  H5F_ACC_EXCL
-- fapl_id = H5F_ACC_RDWR   H5F_ACC_RDONLY
foreign import ccall "hdf5.h H5Fopen"
        c_H5Fopen :: CString -> H5OpenMode -> CInt -> IO H5Handle

-- ---------------------------------------------------------------------------------------
--hid_t  H5Freopen               ( hid_t file_id ) 
foreign import ccall "hdf5.h H5Freopen"
        c_H5Freopen :: H5Handle -> IO H5Handle

-- ---------------------------------------------------------------------------------------
--herr_t H5Fclose                ( hid_t file_id ) 
foreign import ccall "hdf5.h H5Fclose"
    c_H5Fclose :: H5Handle -> IO CInt

-- ---------------------------------------------------------------------------------------
--herr_t H5Fflush                ( hid_t object_id, H5F_scope_t scope ) 
foreign import ccall "hdf5.h H5Fflush"
        c_H5Fflush :: H5Handle -> H5Scope -> IO CInt

--htri_t H5Fis_hdf5(const char *name  ) 
--herr_t H5Fmount(hid_t loc_id, const char *name, hid_t child_id, hid_t plist_id  ) 
--herr_t H5Funmount(hid_t loc_id, const char *name  ) 
--herr_t H5Fget_vfd_handle(hid_t file_id, hid_t fapl_id, void **file_handle  ) 
--herr_t H5Fget_filesize(hid_t file_id, hsize_t *size  ) 
--hid_t  H5Fget_create_plist(hid_t file_id  ) 
--herr_t H5Fflush(hid_t object_id, H5F_scope_t scope  ) 
--herr_t H5Fget_info( hid_t obj_id, H5F_info_t *file_info  ) 
--herr_t H5Fget_intent(hid_t file_id, unsigned *intent) 
--    ssize_t H5Fget_name(hid_t obj_id, char *name, size_t size  ) 
--ssize_t H5Fget_obj_count( hid_t file_id, unsigned int types  ) 



-- ---------------------------------------------------------------------------------------
--herr_t H5LTget_dataset_ndims   ( hid_t loc_id, const char *dset_name, int *rank )
foreign import ccall "hdf5.h H5LTget_dataset_ndims"
    c_H5LTget_dataset_ndims :: H5Handle -> CString -> Ptr CInt -> IO CInt

-- ---------------------------------------------------------------------------------------
--herr_t H5LTget_dataset_info    ( hid_t loc_id, const char *dset_name, hsize_t *dims, H5T_class_t *class_id, size_t *type_size )
foreign import ccall "hdf5.h H5LTget_dataset_info"
         c_H5LTget_dataset_info :: H5Handle -> CString -> Ptr CInt -> Ptr H5TypeClass -> Ptr CInt -> IO CInt

-- ---------------------------------------------------------------------------------------
--herr_t H5LTfind_dataset       (hid_t loc_id, const char *dset_name )
foreign import ccall "hdf5.h H5LTfind_dataset"
        c_H5LTfind_dataset :: H5Handle -> CString -> IO CInt

--herr_t H5LTread_dataset_string(hid_t loc_id, const char *dset_name, char *buffer )
foreign import ccall "hdf5.h H5LTread_dataset_string"
        c_H5LTread_dataset_string :: H5Handle -> CString -> CString -> IO CInt

--herr_t H5LTread_dataset_double(hid_t loc_id, const char *dset_name, double *buffer )
foreign import ccall "hdf5.h H5LTread_dataset_double"
        c_H5LTread_dataset_double :: H5Handle -> CString -> Ptr CDouble -> IO CInt

--herr_t H5LTread_dataset_float (hid_t loc_id, const char *dset_name, float *buffer )
foreign import ccall "hdf5.h H5LTread_dataset_float"
        c_H5LTread_dataset_float :: H5Handle -> CString -> Ptr CFloat -> IO CInt

--herr_t H5LTread_dataset_long  (hid_t loc_id, const char *dset_name, long *buffer )
foreign import ccall "hdf5.h H5LTread_dataset_long"
        c_H5LTread_dataset_long :: H5Handle -> CString -> Ptr CLong -> IO CInt

--herr_t H5LTread_dataset_int   (hid_t loc_id, const char *dset_name, int *buffer )
foreign import ccall "hdf5.h H5LTread_dataset_int"
        c_H5LTread_dataset_int :: H5Handle -> CString -> Ptr CInt -> IO CInt

--herr_t H5LTread_dataset_short (hid_t loc_id, const char *dset_name, short *buffer )
foreign import ccall "hdf5.h H5LTread_dataset_short"
        c_H5LTread_dataset_short :: H5Handle -> CString -> Ptr CShort -> IO CInt

--herr_t H5LTread_dataset_char  (hid_t loc_id, const char *dset_name, char *buffer )
foreign import ccall "hdf5.h H5LTread_dataset_char"
        c_H5LTread_dataset_char :: H5Handle -> CString -> Ptr CChar -> IO CInt

-- ---------------

--herr_t H5LTmake_dataset (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, hid_t type_id, const void *buffer ) 
foreign import ccall "hdf5.h H5LTmake_dataset"
        c_H5LTmake_datase :: H5Handle -> CString -> CInt -> Ptr CInt -> Ptr CChar -> IO CInt

--herr_t H5LTmake_dataset_char  (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const char *buffer )
foreign import ccall "hdf5.h H5LTmake_dataset_char"
        c_H5LTmake_dataset_char :: H5Handle -> CString -> CInt -> Ptr CInt -> Ptr CChar -> IO CInt

--herr_t H5LTmake_dataset_short (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const short *buffer ) 
foreign import ccall "hdf5.h H5LTmake_dataset_short"
        c_H5LTmake_dataset_short  :: H5Handle -> CString -> CInt -> Ptr CInt -> Ptr CShort -> IO CInt

--herr_t H5LTmake_dataset_int   (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const int *buffer ) 
foreign import ccall "hdf5.h H5LTmake_dataset_int"
        c_H5LTmake_dataset_int :: H5Handle -> CString -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

--herr_t H5LTmake_dataset_long  (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const long *buffer )
foreign import ccall "hdf5.h H5LTmake_dataset_long"
        c_H5LTmake_dataset_long :: H5Handle -> CString -> CInt -> Ptr CInt -> Ptr CLong -> IO CInt

--herr_t H5LTmake_dataset_float (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const float *buffer )
foreign import ccall "hdf5.h H5LTmake_dataset_float"
        c_H5LTmake_dataset_float :: H5Handle -> CString -> CInt -> Ptr CInt -> Ptr CFloat -> IO CInt

--herr_t H5LTmake_dataset_double(hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const double *buffer )
foreign import ccall "hdf5.h H5LTmake_dataset_double"
        c_H5LTmake_dataset_double :: H5Handle -> CString -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

--herr_t H5LTmake_dataset_string(hid_t loc_id, const char *dset_name, const char *buffer )
foreign import ccall "hdf5.h H5LTmake_dataset_double"
        c_H5LTmake_dataset_string :: H5Handle -> CString -> CString -> IO CInt









