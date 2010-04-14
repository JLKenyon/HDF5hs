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

module HDF5hs.LowLevel.H5G where

import HDF5hs.LowLevel.H5Types

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Marshal.Array


-- herr_t H5Gclose(hid_t group_id) 
foreign import ccall "hdf5.h H5Gclose"
        c_H5Gclose :: H5Handle -> IO CInt



-- hid_t H5Gcreate1( hid_t loc_id, const char *name, size_t size_hint ) 
foreign import ccall "hdf5.h H5Gcreate1"
        c_H5Gcreate :: H5Handle -> CString -> CULLong -> IO H5Handle
--        c_H5Gcreate1 :: H5Handle -> CString -> CULLong -> IO H5Handle



-- hid_t H5Gcreate2( hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id ) 
foreign import ccall "hdf5.h H5Gcreate2"
        c_H5Gcreate2 :: H5Handle -> CString -> CInt -> CInt -> CInt -> IO H5Handle



-- hid_t H5Gcreate_anon( hid_t loc_id, hid_t gcpl_id, hid_t gapl_id ) 
foreign import ccall "hdf5.h H5Gcreate_anon"
        c_H5Gcreate_anon :: H5Handle -> CInt -> CInt -> IO CInt



-- int H5Gget_comment(hid_t loc_id, const char *name, size_t bufsize, char *comment ) 
foreign import ccall "hdf5.h H5Gget_comment"
        c_H5Gget_comment :: H5Handle -> CString -> CULLong -> CString -> IO CInt



-- hid_t H5Gget_create_plist(hid_t group_id)
foreign import ccall "hdf5.h H5Gget_create_plist"
        c_H5Gget_create_plist :: H5Handle -> IO H5Handle



-- herr_t H5Gget_info( hid_t group_id, H5G_info_t *group_info ) 
{--
foreign import ccall "hdf5.h H5Gget_info"
        c_H5Gget_info :: H5Handle -> H5G_info_t -> IO CInt
--}


-- herr_t H5Gget_info_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, hsize_t n, H5G_info_t *group_info, hid_t lapl_id ) 
{--
foreign import ccall "hdf5.h H5Gget_info_by_idx"
        c_H5Gget_info_by_idx :: H5Handle -> CString -> H5_index_t -> H5_iter_order_t -> CULLong -> Ptr H5G_info_t -> CInt -> IO CInt
--}



-- herr_t H5Gget_info_by_name( hid_t loc_id, const char *group_name, H5G_info_t *group_info, hid_t lapl_id ) 
{--
foreign import ccall "hdf5.h H5Gget_info_by_name"
        c_H5Gget_info_by_name :: H5Handle -> CString -> H5G_info_t -> CInt -> IO CInt
--}


-- herr_t H5Gget_linkval( hid_t loc_id, const char *name, size_t size, char *value ) 
foreign import ccall "hdf5.h H5Gget_linkval"
        c_H5Gget_linkval :: H5Handle -> CString -> CULLong -> CString -> IO CInt



-- herr_t H5Gget_num_objs(hid_t loc_id, hsize_t* num_obj)
foreign import ccall "hdf5.h H5Gget_num_objs"
        c_H5Gget_num_objs :: H5Handle -> Ptr CULLong -> IO CInt



-- herr_t H5Gget_objinfo(hid_t loc_id, const char *name, hbool_t follow_link, H5G_stat_t *statbuf ) 
{--
foreign import ccall "hdf5.h H5Gget_objinfo"
        c_H5Gget_objinfo :: H5Handle -> CString -> CInt -> Ptr H5G_stat_t -> IO CInt
--}


-- ssize_t H5Gget_objname_by_idx(hid_t loc_id, hsize_t idx, char *name, size_t size )
foreign import ccall "hdf5.h H5Gget_objname_by_idx"
        c_H5Gget_objname_by_idx :: H5Handle -> CULLong -> CString -> CULLong -> IO CULLong



-- int H5Gget_objtype_by_idx( hid_t loc_id, hsize_t idx )
foreign import ccall "hdf5.h H5Gget_objtype_by_idx"
        c_H5Gget_objtype_by_idx :: H5Handle -> CULLong -> IO CInt



-- int H5Giterate(hid_t loc_id, const char *name, int *idx, H5G_iterate_t operator, void *operator_data ) 
{--
foreign import ccall "hdf5.h H5Giterate"
        c_H5Giterate :: H5Handle -> CChar -> CInt -> H5G_iterate_t -> () -> IO CInt
--}


-- herr_t H5Glink(hid_t loc_id, H5G_link_t link_type, const char *current_name, const char *new_name ) 
{--
foreign import ccall "hdf5.h H5Glink"
        c_H5Glink :: H5Handle -> H5G_link_t -> CString -> CString -> IO CInt
--}


-- herr_t H5Glink2( hid_t curr_loc_id, const char *current_name, H5G_link_t link_type, hid_t new_loc_id, const char *new_name ) 
{--
foreign import ccall "hdf5.h H5Glink2"
        c_H5Glink2 :: H5Handle -> CString -> H5G_link_t -> CInt -> CString -> IO CInt
--}


-- herr_t H5Gmove(hid_t loc_id, const char *src_name, const char *dst_name ) 
foreign import ccall "hdf5.h H5Gmove"
        c_H5Gmove :: H5Handle -> CChar -> CChar -> IO CInt



-- herr_t H5Gmove2( hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, const char *dst_name ) 
foreign import ccall "hdf5.h H5Gmove2"
        c_H5Gmove2 :: H5Handle -> CChar -> H5Handle -> CChar -> IO CInt



{-- -- Macro, no such function, binds to open1 or open2
-- hid_t H5Gopen( hid_t loc_id, const char *name ) 
foreign import ccall "hdf5.h H5Gopen"
        c_H5Gopen :: H5Handle -> CString -> IO CInt
--}


---- hid_t H5Gopen( hid_t loc_id, const char * name, hid_t gapl_id ) 
--foreign import ccall "hdf5.h H5Gopen"
--        c_H5Gopen :: H5Handle -> CString -> CInt -> IO CInt



-- hid_t H5Gopen1(hid_t loc_id, const char *name ) 
foreign import ccall "hdf5.h H5Gopen1"
        c_H5Gopen :: H5Handle -> CString -> IO H5Handle
--        c_H5Gopen1 :: H5Handle -> CString -> IO CInt



-- hid_t H5Gopen2( hid_t loc_id, const char * name, hid_t gapl_id ) 
foreign import ccall "hdf5.h H5Gopen2"
        c_H5Gopen2 :: H5Handle -> CString -> CInt -> IO CInt



-- herr_t H5Gset_comment(hid_t loc_id, const char *name, const char *comment ) 
foreign import ccall "hdf5.h H5Gset_comment"
        c_H5Gset_comment :: H5Handle -> CString -> CString -> IO CInt



-- herr_t H5Gunlink(hid_t loc_id, const char *name ) 
foreign import ccall "hdf5.h H5Gunlink"
        c_H5Gunlink :: H5Handle -> CString -> IO CInt



