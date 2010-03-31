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

module HDF5hs.LowLevel.H5F where

import HDF5hs.LowLevel.H5Types



-- herr_t H5Fclose( hid_t file_id )
foreign import ccall "hdf5.h H5Fclose"
        c_H5Fclose :: CInt -> IO CInt



-- hid_t H5Fcreate( const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id )
foreign import ccall "hdf5.h H5Fcreate"
        c_H5Fcreate :: CString -> CInt -> CInt -> CInt -> IO CInt



-- herr_t H5Fflush(hid_t object_id, H5F_scope_t scope )
foreign import ccall "hdf5.h H5Fflush"
        c_H5Fflush :: CInt -> H5F_scope_t -> IO CInt



-- hid_t H5Fget_access_plist(hid_t file_id)
foreign import ccall "hdf5.h H5Fget_access_plist"
        c_H5Fget_access_plist :: CInt -> IO CInt



-- hid_t H5Fget_create_plist(hid_t file_id )
foreign import ccall "hdf5.h H5Fget_create_plist"
        c_H5Fget_create_plist :: CInt -> IO CInt



-- herr_t H5Fget_filesize(hid_t file_id, hsize_t *size )
foreign import ccall "hdf5.h H5Fget_filesize"
        c_H5Fget_filesize :: CInt -> Ptr CULLong -> IO CInt



-- hssize_t H5Fget_freespace(hid_t file_id)
foreign import ccall "hdf5.h H5Fget_freespace"
        c_H5Fget_freespace :: CInt -> IO CULLong



-- herr_t H5Fget_info( hid_t obj_id, H5F_info_t *file_info ) 
foreign import ccall "hdf5.h H5Fget_info"
        c_H5Fget_info :: CInt -> Ptr H5F_info_t -> IO CInt



-- herr_t H5Fget_intent(hid_t file_id, unsigned *intent) 
foreign import ccall "hdf5.h H5Fget_intent"
        c_H5Fget_intent :: CInt -> Ptr CInt -> IO CInt



-- herr_t H5Fget_mdc_config(hid_tfile_id, H5AC_cache_config_t *config_ptr)
foreign import ccall "hdf5.h H5Fget_mdc_config"
        c_H5Fget_mdc_config :: CInt -> Ptr H5AC_cache_config_t -> IO CInt



-- herr_t H5Fget_mdc_hit_rate(hid_tfile_id, double *hit_rate_ptr)
foreign import ccall "hdf5.h H5Fget_mdc_hit_rate"
        c_H5Fget_mdc_hit_rate :: CInt -> Ptr CDouble -> IO CInt



-- herr_t H5Fget_mdc_size(hid_t file_id, size_t *max_size_ptr, size_t *min_clean_size_ptr, size_t *cur_size_ptr, int *cur_num_entries_ptr)
foreign import ccall "hdf5.h H5Fget_mdc_size"
        c_H5Fget_mdc_size :: CInt -> Ptr CULLong -> Ptr CULLong -> Ptr CULLong -> Ptr CInt -> IO CInt



-- ssize_t H5Fget_name(hid_t obj_id, char *name, size_t size )
foreign import ccall "hdf5.h H5Fget_name"
        c_H5Fget_name :: CInt -> CString -> CULLong -> IO CULLong



-- ssize_t H5Fget_obj_count( hid_t file_id,	 unsigned int types )
foreign import ccall "hdf5.h H5Fget_obj_count"
        c_H5Fget_obj_count :: CInt -> CInt -> IO CULLong



-- ssize_t H5Fget_obj_ids( hid_t file_id, unsigned int types, size_t max_objs, hid_t *obj_id_list )
foreign import ccall "hdf5.h H5Fget_obj_ids"
        c_H5Fget_obj_ids :: CInt -> CInt -> CULLong -> Ptr CInt -> IO CULLong



-- herr_t H5Fget_vfd_handle(hid_t file_id, hid_t fapl_id, void **file_handle )
foreign import ccall "hdf5.h H5Fget_vfd_handle"
        c_H5Fget_vfd_handle :: CInt -> CInt -> Ptr () -> IO CInt



-- htri_t H5Fis_hdf5(const char *name )
foreign import ccall "hdf5.h H5Fis_hdf5"
        c_H5Fis_hdf5 :: CString -> IO CInt



-- herr_t H5Fmount(hid_t loc_id, const char *name, hid_t child_id, hid_t plist_id )
foreign import ccall "hdf5.h H5Fmount"
        c_H5Fmount :: CInt -> CString -> CInt -> CInt -> IO CInt



-- hid_t H5Fopen( const char *name, unsigned flags, hid_t fapl_id )
foreign import ccall "hdf5.h H5Fopen"
        c_H5Fopen :: CString -> CInt -> CInt -> IO CInt



-- hid_t H5Freopen(hid_t file_id )
foreign import ccall "hdf5.h H5Freopen"
        c_H5Freopen :: CInt -> IO CInt



-- herr_t H5Freset_mdc_hit_rate_stats(hid_tfile_id)
foreign import ccall "hdf5.h H5Freset_mdc_hit_rate_stats"
        c_H5Freset_mdc_hit_rate_stats :: CInt -> IO CInt



-- herr_t H5Fset_mdc_config(hid_tfile_id, H5AC_cache_config_t *config_ptr)
foreign import ccall "hdf5.h H5Fset_mdc_config"
        c_H5Fset_mdc_config :: CInt -> Ptr H5AC_cache_config_t -> IO CInt



-- herr_t H5Funmount(hid_t loc_id, const char *name )
foreign import ccall "hdf5.h H5Funmount"
        c_H5Funmount :: CInt -> CString -> IO CInt



