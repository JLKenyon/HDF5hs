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

{-# LANGUAGE CPP,ForeignFunctionInterface #-}

module HDF5hs.LowLevel.H5D where

import HDF5hs.LowLevel.H5Types

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Marshal.Array


-- herr_t H5Dclose(hid_t dataset_id )
foreign import ccall "hdf5.h H5Dclose"
        c_H5Dclose :: H5Handle -> IO CInt



{-- -- No such function, just a macro
-- hid_t H5Dcreate( hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t dcpl_id ) 
foreign import ccall "hdf5.h H5Dcreate"
        c_H5Dcreate :: H5Handle -> CString -> CInt -> CInt -> CInt -> IO H5Handle
--}


---- hid_t H5Dcreate( hid_t loc_id, const char *name, hid_t dtype_id, hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id ) 
--foreign import ccall "hdf5.h H5Dcreate"
--        c_H5Dcreate :: H5Handle -> CString -> CInt -> CInt -> CInt -> CInt -> CInt -> IO H5Handle



-- hid_t H5Dcreate1( hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t dcpl_id )
foreign import ccall "hdf5.h H5Dcreate1"
        c_H5Dcreate :: H5Handle -> CString -> H5Handle -> H5Handle -> H5Handle -> IO H5Handle
--        c_H5Dcreate1 :: H5Handle -> CString -> CInt -> CInt -> CInt -> IO H5Handle



-- hid_t H5Dcreate2( hid_t loc_id, const char *name, hid_t dtype_id, hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id ) 
foreign import ccall "hdf5.h H5Dcreate2"
        c_H5Dcreate2 :: H5Handle -> CString -> CInt -> CInt -> CInt -> CInt -> CInt -> IO H5Handle



-- hid_t H5Dcreate_anon( hid_t loc_id, hid_t type_id, hid_t space_id, hid_t dcpl_id, hid_t dapl_id )
foreign import ccall "hdf5.h H5Dcreate_anon"
        c_H5Dcreate_anon :: H5Handle -> CInt -> CInt -> CInt -> CInt -> IO H5Handle



-- herr_t H5Dextend( hid_t dataset_id, const hsize_t size[] )
foreign import ccall "hdf5.h H5Dextend"
        c_H5Dextend :: H5Handle -> CULLong -> IO CInt



-- herr_t H5Dfill( const void *fill, hid_t fill_type_id, void *buf, hid_t buf_type_id, hid_t space_id )
foreign import ccall "hdf5.h H5Dfill"
        c_H5Dfill :: Ptr () -> CInt -> Ptr () -> CInt -> CInt -> IO CInt



-- hid_t H5Dget_access_plist( hid_t dataset_id ) 
foreign import ccall "hdf5.h H5Dget_access_plist"
        c_H5Dget_access_plist :: H5Handle -> IO H5Handle



-- hid_t H5Dget_create_plist(hid_t dataset_id )
foreign import ccall "hdf5.h H5Dget_create_plist"
        c_H5Dget_create_plist :: H5Handle -> IO H5Handle



-- haddr_t H5Dget_offset(hid_t dset_id)
foreign import ccall "hdf5.h H5Dget_offset"
        c_H5Dget_offset :: H5Handle -> IO CInt



-- hid_t H5Dget_space(hid_t dataset_id )
foreign import ccall "hdf5.h H5Dget_space"
        c_H5Dget_space :: H5Handle -> IO H5Handle



-- herr_t H5Dget_space_status(hid_t dset_id, H5D_space_status_t *status)
foreign import ccall "hdf5.h H5Dget_space_status"
        c_H5Dget_space_status :: H5Handle -> Ptr H5D_space_status_t -> IO CInt



-- hsize_t H5Dget_storage_size(hid_t dataset_id )
foreign import ccall "hdf5.h H5Dget_storage_size"
        c_H5Dget_storage_size :: H5Handle -> IO CULLong



-- hid_t H5Dget_type(hid_t dataset_id )
foreign import ccall "hdf5.h H5Dget_type"
        c_H5Dget_type :: H5Handle -> IO H5Handle



-- herr_t H5Diterate( void *buf, hid_t type_id, hid_t space_id, H5D_operator_t operator, void *operator_data )
{--
foreign import ccall "hdf5.h H5Diterate"
        c_H5Diterate :: Ptr () -> CInt -> CInt -> H5D_operator_t -> Ptr () -> IO CInt
--}


{-- -- No such function, just a macro
-- hid_t H5Dopen( hid_t loc_id, const char *name ) 
foreign import ccall "hdf5.h H5Dopen"
        c_H5Dopen :: H5Handle -> CString -> IO CInt
--}


---- hid_t H5Dopen( hid_t loc_id, const char *name, hid_t dapl_id ) 
--foreign import ccall "hdf5.h H5Dopen"
--        c_H5Dopen :: H5Handle -> CString -> CInt -> IO CInt



-- hid_t H5Dopen1( hid_t loc_id, const char *name )
foreign import ccall "hdf5.h H5Dopen1"
        c_H5Dopen :: H5Handle -> CString -> IO CInt



-- hid_t H5Dopen2( hid_t loc_id, const char *name, hid_t dapl_id ) 
foreign import ccall "hdf5.h H5Dopen2"
        c_H5Dopen2 :: H5Handle -> CString -> CInt -> IO H5Handle



-- herr_t H5Dread(hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t xfer_plist_id, void * buf ) 
foreign import ccall "hdf5.h H5Dread"
        c_H5Dread :: H5Handle -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO CInt



-- herr_t H5Dset_extent( hid_t dset_id, 	const hsize_t size[] ) 
foreign import ccall "hdf5.h H5Dset_extent"
        c_H5Dset_extent :: H5Handle -> CULLong -> IO CInt



-- herr_t H5Dvlen_get_buf_size(hid_t dataset_id, hid_t type_id, hid_t space_id, hsize_t *size )
foreign import ccall "hdf5.h H5Dvlen_get_buf_size"
        c_H5Dvlen_get_buf_size :: H5Handle -> CInt -> CInt -> CULLong -> IO CInt



-- herr_t H5Dvlen_reclaim(hid_t type_id, hid_t space_id, hid_t plist_id, void *buf )
foreign import ccall "hdf5.h H5Dvlen_reclaim"
        c_H5Dvlen_reclaim :: H5Handle -> CInt -> CInt -> Ptr () -> IO CInt



-- herr_t H5Dwrite(hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t xfer_plist_id, const void * buf ) 
foreign import ccall "hdf5.h H5Dwrite"
        c_H5Dwrite :: H5Handle -> H5Handle -> H5Handle -> H5Handle -> H5Handle -> Ptr CInt -> IO CInt



