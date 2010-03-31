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

module HDF5hs.LowLevel.H5A where

import HDF5hs.LowLevel.H5Types


-- herr_t H5Aclose(hid_t attr_id)
foreign import ccall "hdf5.h H5Aclose"
        c_H5Aclose :: CInt -> IO CInt

-- hid_t H5Acreate1( hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id )
foreign import ccall "hdf5.h H5Acreate1"
        c_H5Acreate1 :: CInt -> CString -> CInt -> CInt -> CInt -> IO CInt

-- hid_t H5Acreate2( hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id, ) 
foreign import ccall "hdf5.h H5Acreate2"
        c_H5Acreate2 :: CInt -> CString -> CInt -> CInt -> CInt -> CInt -> IO CInt

-- hid_t H5Acreate_by_name( hid_t loc_id, const char *obj_name, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id, hid_t lapl_id ) 
foreign import ccall "hdf5.h H5Acreate_by_name"
        c_H5Acreate_by_name :: CInt -> CString -> CString -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt

-- herr_t H5Adelete( hid_t loc_id, const char *attr_name )
foreign import ccall "hdf5.h H5Adelete"
        c_H5Adelete :: CInt -> CString -> IO CInt

-- herr_t H5Adelete_by_name( hid_t loc_id, const char *obj_name, const char *attr_name, hid_t lapl_id ) 
foreign import ccall "hdf5.h H5Adelete_by_name"
        c_H5Adelete_by_name :: CInt -> CString -> CString -> CInt -> IO CInt

-- herr_t H5Adelete_by_idx( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t lapl_id ) 
foreign import ccall "hdf5.h H5Adelete_by_idx"
        c_H5Adelete_by_idx :: CInt -> CString -> H5_index_t -> H5_iter_order_t -> CULLong -> CInt -> IO CInt

-- htri_t H5Aexists( hid_t obj_id, const char *attr_name ) 
foreign import ccall "hdf5.h H5Aexists"
        c_H5Aexists :: CInt -> CString -> IO CInt

-- htri_t H5Aexists_by_name( hid_t loc_id, const char *obj_name, const char *attr_name, hid_t lapl_id ) 
foreign import ccall "hdf5.h H5Aexists_by_name"
        c_H5Aexists_by_name :: CInt -> CString -> CString -> CInt -> IO CInt

-- hid_t H5Aget_create_plist(hid_t attr_id)
foreign import ccall "hdf5.h H5Aget_create_plist"
        c_H5Aget_create_plist :: CInt -> IO CInt

-- herr_t H5Aget_info( hid_t attr_id, H5A_info_t *ainfo ) 
foreign import ccall "hdf5.h H5Aget_info"
        c_H5Aget_info :: CInt -> Ptr H5A_info_t -> IO CInt

-- herr_t H5Aget_info_by_idx( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5A_info_t *ainfo, hid_t lapl_id ) 
foreign import ccall "hdf5.h H5Aget_info_by_idx"
        c_H5Aget_info_by_idx :: CInt -> CString -> H5_index_t -> H5_iter_order_t -> CULLong -> Ptr H5A_info_t -> CInt -> IO CInt

-- herr_t H5Aget_info_by_name( hid_t loc_id, const char *obj_name, const char *attr_name, H5A_info_t *ainfo, hid_t lapl_id ) 
foreign import ccall "hdf5.h H5Aget_info_by_name"
        c_H5Aget_info_by_name :: CInt -> CString -> CString -> Ptr H5A_info_t -> CInt -> IO CInt

-- ssize_t H5Aget_name(hid_t attr_id, size_t buf_size, char *buf )
foreign import ccall "hdf5.h H5Aget_name"
        c_H5Aget_name :: CInt -> CULLong -> CString -> IO CULLong

-- ssize_t H5Aget_name_by_idx( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, char *name, size_t size, hid_t lapl_id ) 
foreign import ccall "hdf5.h H5Aget_name_by_idx"
        c_H5Aget_name_by_idx :: CInt -> CString -> H5_index_t -> H5_iter_order_t -> CULLong -> CString -> CULLong -> CInt -> IO CULLong

-- int H5Aget_num_attrs(hid_t loc_id)
foreign import ccall "hdf5.h H5Aget_num_attrs"
        c_H5Aget_num_attrs :: CInt -> IO CInt

-- hid_t H5Aget_space(hid_t attr_id)
foreign import ccall "hdf5.h H5Aget_space"
        c_H5Aget_space :: CInt -> IO CInt

-- hsize_t H5Aget_storage_size(hid_tattr_id)
foreign import ccall "hdf5.h H5Aget_storage_size"
        c_H5Aget_storage_size :: CInt -> IO CULLong

-- hid_t H5Aget_type(hid_t attr_id)
foreign import ccall "hdf5.h H5Aget_type"
        c_H5Aget_type :: CInt -> IO CInt

-- herr_t H5Aiterate1( hid_t loc_id, unsigned * idx, H5A_operator1_t op, void *op_data )
foreign import ccall "hdf5.h H5Aiterate1"
        c_H5Aiterate1 :: CInt -> Ptr CInt -> H5A_operator1_t -> Ptr () -> IO CInt

-- herr_t H5Aiterate2( hid_t obj_id, H5_index_t idx_type, H5_iter_order_t order, hsize_t *n, H5A_operator2_t op, void *op_data, ) 
foreign import ccall "hdf5.h H5Aiterate2"
        c_H5Aiterate2 :: CInt -> H5_index_t -> H5_iter_order_t -> Ptr CULLong -> H5A_operator2_t -> Ptr () -> IO CInt

-- herr_t H5Aiterate_by_name( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t *n, H5A_operator2_t op, void *op_data, hid_t lapd_id ) 
foreign import ccall "hdf5.h H5Aiterate_by_name"
        c_H5Aiterate_by_name :: CInt -> CString -> H5_index_t -> H5_iter_order_t -> Ptr CULLong -> H5A_operator2_t -> Ptr () -> CInt -> IO CInt

-- hid_t H5Aopen( hid_t obj_id, const char *attr_name, hid_t aapl_id ) 
foreign import ccall "hdf5.h H5Aopen"
        c_H5Aopen :: CInt -> CString -> CInt -> IO CInt

-- hid_t H5Aopen_by_idx( hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t aapl_id, hid_t lapl_id ) 
foreign import ccall "hdf5.h H5Aopen_by_idx"
        c_H5Aopen_by_idx :: CInt -> CString -> H5_index_t -> H5_iter_order_t -> CULLong -> CInt -> CInt -> IO CInt

-- hid_t H5Aopen_by_name( hid_t loc_id, const char *obj_name, const char *attr_name, hid_t aapl_id, hid_t lapl_id ) 
foreign import ccall "hdf5.h H5Aopen_by_name"
        c_H5Aopen_by_name :: CInt -> CString -> CString -> CInt -> CInt -> IO CInt

-- hid_t H5Aopen_idx(hid_t loc_id, unsigned int idx )
foreign import ccall "hdf5.h H5Aopen_idx"
        c_H5Aopen_idx :: CInt -> CInt -> IO CInt

-- hid_t H5Aopen_name(hid_t loc_id, const char *name )
foreign import ccall "hdf5.h H5Aopen_name"
        c_H5Aopen_name :: CInt -> CString -> IO CInt

-- herr_t H5Aread(hid_t attr_id, hid_t mem_type_id, void *buf )
foreign import ccall "hdf5.h H5Aread"
        c_H5Aread :: CInt -> CInt -> Ptr () -> IO CInt

-- herr_t H5Arename( hid_t loc_id, char *old_attr_name, char *new_attr_name ) 
foreign import ccall "hdf5.h H5Arename"
        c_H5Arename :: CInt -> CString -> CString -> IO CInt

-- herr_t H5Arename_by_name( hid_t loc_id, const char *obj_name, const char *old_attr_name, const char *new_attr_name, hid_t lapl_id ) 
foreign import ccall "hdf5.h H5Arename_by_name"
        c_H5Arename_by_name :: CInt -> CString -> CString -> CString -> CInt -> IO CInt

-- herr_t H5Awrite(hid_t attr_id, hid_t mem_type_id, const void *buf )
foreign import ccall "hdf5.h H5Awrite"
        c_H5Awrite :: CInt -> CInt -> Ptr () -> IO CInt
