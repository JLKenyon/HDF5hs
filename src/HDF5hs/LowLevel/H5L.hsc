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

module HDF5hs.LowLevel.H5L where

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

#{enum H5L_type_t, H5L_type_t
 , h5L_TYPE_ERROR    = H5L_TYPE_ERROR   
 , h5L_TYPE_HARD     = H5L_TYPE_HARD    
 , h5L_TYPE_SOFT     = H5L_TYPE_SOFT    
 , h5L_TYPE_EXTERNAL = H5L_TYPE_EXTERNAL
 , h5L_TYPE_MAX      = H5L_TYPE_MAX     
 }

#{enum H5T_cset_t, H5T_cset_t
 , h5T_CSET_ERROR       = H5T_CSET_ERROR	
 , h5T_CSET_ASCII       = H5T_CSET_ASCII	 
 , h5T_CSET_UTF8        = H5T_CSET_UTF8	 
 , h5T_CSET_RESERVED_2	= H5T_CSET_RESERVED_2	
 , h5T_CSET_RESERVED_3	= H5T_CSET_RESERVED_3	
 , h5T_CSET_RESERVED_4	= H5T_CSET_RESERVED_4	
 , h5T_CSET_RESERVED_5	= H5T_CSET_RESERVED_5	
 , h5T_CSET_RESERVED_6	= H5T_CSET_RESERVED_6	
 , h5T_CSET_RESERVED_7	= H5T_CSET_RESERVED_7	
 , h5T_CSET_RESERVED_8	= H5T_CSET_RESERVED_8	
 , h5T_CSET_RESERVED_9	= H5T_CSET_RESERVED_9	
 , h5T_CSET_RESERVED_10 = H5T_CSET_RESERVED_10 
 , h5T_CSET_RESERVED_11 = H5T_CSET_RESERVED_11 
 , h5T_CSET_RESERVED_12 = H5T_CSET_RESERVED_12 
 , h5T_CSET_RESERVED_13 = H5T_CSET_RESERVED_13 
 , h5T_CSET_RESERVED_14 = H5T_CSET_RESERVED_14 
 , h5T_CSET_RESERVED_15 = H5T_CSET_RESERVED_15 
} 
 
data H5L_info_t = H5L_info_t {
    unType	   :: H5L_type_t
  , unCorder_valid :: Bool
  , unCorder	   :: Int64 
  , unCSet	   :: H5T_cset_t
  , unAddress	   :: Haddr_t
} 

newtype Haddr_t = Haddr_t { unHaddr_T :: Int64 }
    deriving (Eq, Show)

#{enum H5_index_t, H5_index_t
 ,  h5_INDEX_UNKNOWN   = H5_INDEX_UNKNOWN
 ,  h5_INDEX_NAME      = H5_INDEX_NAME
 ,  h5_INDEX_CRT_ORDER = H5_INDEX_CRT_ORDER
 ,  h5_INDEX_N	       = H5_INDEX_N
}

#{enum H5_iter_order_t, H5_iter_order_t
 , h5_ITER_UNKNOWN = H5_ITER_UNKNOWN
 , h5_ITER_INC	   = H5_ITER_INC	  
 , h5_ITER_DEC	   = H5_ITER_DEC	  
 , h5_ITER_NATIVE  = H5_ITER_NATIVE 
 , h5_ITER_N	   = H5_ITER_N	  
} 

-- This class cannot be implemented
--#{enum H5L_class_t, H5L_class_t
-- , int                 version
-- , H5L_type_t          id
-- , const char *        comment
-- , H5L_create_func_t   create_func
-- , H5L_move_func_t     move_func
-- , H5L_copy_func_t     copy_func
-- , H5L_traverse_func_t trav_func
-- , H5L_delete_func_t   del_func
-- , H5L_query_func_t    query_func
--} 

 
-- -----------------------------------------------------------------------------


-- herr_t H5Lcopy( hid_t src_loc_id, const char *src_name, hid_t dest_loc_id, const char *dest_name, hid_t lcpl_id  hid_t lapl_id  )
foreign import ccall "hdf5.h H5Lcopy"
        c_H5Lcopy :: CInt -> CString -> CInt -> CString -> CInt -> IO CInt



-- herr_t H5Lcreate_external( const char *file_name, const char *object_name, hid_t link_loc_id, const char *link_name, hid_t lcpl_id, hid_t lapl_id )
foreign import ccall "hdf5.h H5Lcreate_external"
        c_H5Lcreate_external :: CString -> CString -> CInt -> CString -> CInt -> CInt -> IO CInt



-- herr_t H5Lcreate_hard( hid_t obj_loc_id, const char *obj_name, hid_t link_loc_id, const char *link_name, hid_t lcpl_id, hid_t lapl_id )
foreign import ccall "hdf5.h H5Lcreate_hard"
        c_H5Lcreate_hard :: CInt -> CString -> CInt -> CString -> CInt -> CInt -> IO CInt



-- herr_t H5Lcreate_soft( const char *target_path, hid_t link_loc_id, const char *link_name, hid_t lcpl_id, hid_t lapl_id )
foreign import ccall "hdf5.h H5Lcreate_soft"
        c_H5Lcreate_soft :: CString -> CInt -> CString -> CInt -> CInt -> IO CInt



-- herr_t H5Lcreate_ud( hid_t link_loc_id, const char *link_name, H5L_type_t link_type, const char *udata, size_t udata_size, hid_t lcpl_id, hid_t lapl_id  )
foreign import ccall "hdf5.h H5Lcreate_ud"
        c_H5Lcreate_ud :: CInt -> CString -> H5L_type_t -> CString -> Ptr CInt -> CInt -> CInt -> IO CInt



-- htri_t H5Lexists( hid_t loc_id, const char *name, hid_t lapl_id  )
foreign import ccall "hdf5.h H5Lexists"
        c_H5Lexists :: CInt -> CString -> CInt -> IO CInt



-- herr_t H5Lget_info( hid_t link_loc_id, const char *link_name, H5L_info_t *link_buff, hid_t lapl_id  )
foreign import ccall "hdf5.h H5Lget_info"
        c_H5Lget_info :: CInt -> CString -> Ptr H5L_info_t -> CInt -> IO CInt



-- herr_t H5Lget_info_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_field, H5_iter_order_t order, hsize_t n, H5L_info_t *link_val, hid_t lapl_id  )
foreign import ccall "hdf5.h H5Lget_info_by_idx"
        c_H5Lget_info_by_idx :: CInt -> CString -> H5_index_t -> H5_iter_order_t -> CInt -> Ptr H5L_info_t -> CInt -> IO CInt



-- ssize_t H5Lget_name_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_field, H5_iter_order_t order, hsize_t n, char *name, size_t size, hid_t lapl_id  )
foreign import ccall "hdf5.h H5Lget_name_by_idx"
        c_H5Lget_name_by_idx :: CInt -> CString -> H5_index_t -> H5_iter_order_t -> CInt -> CString -> Ptr CInt -> CInt -> IO CInt



-- herr_t H5Lget_val( hid_t link_loc_id, const char *link_name, void *linkval_buff, size_t size, hid_t lapl_id  )
foreign import ccall "hdf5.h H5Lget_val"
        c_H5Lget_val :: CInt -> CString -> Ptr () -> Ptr CInt -> CInt -> IO CInt



-- herr_t H5Lget_val_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, hsize_t n, void *link_val, size_t size, hid_t lapl_id  )
foreign import ccall "hdf5.h H5Lget_val_by_idx"
        c_H5Lget_val_by_idx :: CInt -> CString -> H5_index_t -> H5_iter_order_t -> CInt -> Ptr () -> Ptr CInt -> CInt -> IO CInt



-- htri_t H5Lis_registered( H5L_type_t link_cls_id  )
foreign import ccall "hdf5.h H5Lis_registered"
        c_H5Lis_registered :: H5L_type_t -> IO CInt



---- herr_t H5Literate( hid_t group_id, H5_index_t index_type, H5_iter_order_t order, hsize_t *idx, H5L_iterate_t op, void *op_data  )
--foreign import ccall "hdf5.h H5Literate"
--        c_H5Literate :: CInt -> H5_index_t -> H5_iter_order_t -> Ptr CInt -> XXXXXXXXXXXXXXX -> Ptr () -> IO CInt



---- herr_t H5Literate_by_name( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, hsize_t *idx, H5L_iterate_t op, void *op_data, hid_t *lapl_id  )
--foreign import ccall "hdf5.h H5Literate_by_name"
--        c_H5Literate_by_name :: CInt -> CString -> H5_index_t -> H5_iter_order_t -> Ptr CInt -> XXXXXXXXXXXXXXX -> Ptr () -> Ptr CInt -> IO CInt



-- herr_t H5Lmove( hid_t src_loc_id, const char *src_name, hid_t dest_loc_id, const char *dest_name, hid_t lcpl, hid_t lapl )
foreign import ccall "hdf5.h H5Lmove"
        c_H5Lmove :: CInt -> CString -> CInt -> CString -> CInt -> CInt -> IO CInt



---- herr_t H5Lregister( const H5L_class_t * link_class  )
--foreign import ccall "hdf5.h H5Lregister"
--        c_H5Lregister :: Ptr H5L_class_t -> IO CInt



-- herr_t H5Ldelete( hid_t loc_id, const char *name, hid_t lapl_id  )
foreign import ccall "hdf5.h H5Ldelete"
        c_H5Ldelete :: CInt -> CString -> CInt -> IO CInt



-- herr_t H5Ldelete_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_field, H5_iter_order_t order, hsize_t n, hid_t lapl_id  )
foreign import ccall "hdf5.h H5Ldelete_by_idx"
        c_H5Ldelete_by_idx :: CInt -> CString -> H5_index_t -> H5_iter_order_t -> CInt -> CInt -> IO CInt



-- herr_t H5Lunpack_elink_val( char *ext_linkval, size_t link_size, unsigned *flags, const char **filename, const char **obj_path  )
foreign import ccall "hdf5.h H5Lunpack_elink_val"
        c_H5Lunpack_elink_val :: CString -> Ptr CInt -> Ptr CInt -> Ptr CString -> Ptr CString -> IO CInt



-- herr_t H5Lunregister( H5L_type_t link_cls_id  )
foreign import ccall "hdf5.h H5Lunregister"
        c_H5Lunregister :: H5L_type_t -> IO CInt



---- herr_t H5Lvisit( hid_t group_id, H5_index_t index_type, H5_iter_order_t order, H5L_iterate_t op, void *op_data  )
--foreign import ccall "hdf5.h H5Lvisit"
--        c_H5Lvisit :: CInt -> H5_index_t -> H5_iter_order_t -> XXXXXXXXXXXXXXX -> Ptr () -> IO CInt



---- herr_t H5Lvisit_by_name( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, H5L_iterate_t op, void *op_data, hid_t lapl_id  )
--foreign import ccall "hdf5.h H5Lvisit_by_name"
--        c_H5Lvisit_by_name :: CInt -> CString -> H5_index_t -> H5_iter_order_t -> XXXXXXXXXXXXXXX -> Ptr () -> CInt -> IO CInt
