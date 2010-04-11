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

{-# LANGUAGE CPP,ForeignFunctionInterface #-}

module HDF5hs.LowLevel.H5T where

import HDF5hs.LowLevel.H5Types

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Marshal.Array


-- hid_t H5Tarray_create1( hid_t base_type_id, int rank, const hsize_t dims[/*rank*/], const int perm[/*rank*/] )
foreign import ccall "hdf5.h H5Tarray_create1"
        c_H5Tarray_create1 :: H5Handle -> CInt -> CULLong -> CInt -> IO H5Handle



-- hid_t H5Tarray_create2( hid_t base_type_id, unsigned rank, const hsize_t dims[/*rank*/], )
foreign import ccall "hdf5.h H5Tarray_create2"
        c_H5Tarray_create2 :: H5Handle -> CInt -> CULLong -> IO H5Handle



-- herr_t H5Tclose( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tclose"
        c_H5Tclose :: H5Handle -> IO CInt



---- herr_t H5Tcommit( hid_t loc_id, const char * name, hid_t dtype_id ) 
--foreign import ccall "hdf5.h H5Tcommit"
--        c_H5Tcommit :: H5Handle -> CString -> H5Handle -> IO CInt
-- 
-- 
-- 
---- hid_t H5Tcommit( hid_t loc_id, const char *name, hid_t dtype_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id ) 
--foreign import ccall "hdf5.h H5Tcommit"
--        c_H5Tcommit :: H5Handle -> CString -> H5Handle -> H5Handle -> H5Handle -> H5Handle -> IO H5Handle



-- herr_t H5Tcommit1( hid_t loc_id, const char * name, hid_t dtype_id )
foreign import ccall "hdf5.h H5Tcommit1"
        c_H5Tcommit :: H5Handle -> CString -> H5Handle -> IO CInt
        --c_H5Tcommit1 :: H5Handle -> CString -> H5Handle -> IO CInt



-- herr_t H5Tcommit2( hid_t loc_id, const char *name, hid_t dtype_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id ) 
foreign import ccall "hdf5.h H5Tcommit2"
        c_H5Tcommit2 :: H5Handle -> CString -> H5Handle -> H5Handle -> H5Handle -> H5Handle -> IO CInt



-- herr_t H5Tcommit_anon( hid_t loc_id, hid_t dtype_id, hid_t tcpl_id, hid_t tapl_id ) 
foreign import ccall "hdf5.h H5Tcommit_anon"
        c_H5Tcommit_anon :: H5Handle -> H5Handle -> H5Handle -> H5Handle -> IO CInt



-- htri_tH5Tcommitted( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tcommitted"
        c_H5Tcommitted :: H5Handle -> IO CInt



-- htri_t H5Tcompiler_conv(hid_t src_id, hid_t dst_id)
foreign import ccall "hdf5.h H5Tcompiler_conv"
        c_H5Tcompiler_conv :: H5Handle -> H5Handle -> IO CInt



-- herr_t H5Tconvert(hid_t src_id, hid_t dst_id, size_t nelmts, void *buf, void *background, hid_t plist_id )
foreign import ccall "hdf5.h H5Tconvert"
        c_H5Tconvert :: H5Handle -> H5Handle -> CULLong -> Ptr () -> Ptr () -> H5Handle -> IO CInt



-- hid_t H5Tcopy(hid_t dtype_id)
foreign import ccall "hdf5.h H5Tcopy"
        c_H5Tcopy :: H5Handle -> IO H5Handle



-- hid_t H5Tcreate(H5T_class_t class, size_tsize )
foreign import ccall "hdf5.h H5Tcreate"
        c_H5Tcreate :: H5T_class_t -> CULLong -> IO H5Handle



-- hid_t H5Tdecode (unsigned char *buf)
foreign import ccall "hdf5.h H5Tdecode"
        c_H5Tdecode :: Ptr CChar -> IO H5Handle



-- htri_t H5Tdetect_class(hid_t dtype_id, H5T_class_tdtype_class )
foreign import ccall "hdf5.h H5Tdetect_class"
        c_H5Tdetect_class :: H5Handle -> H5T_class_t -> IO CInt



-- herr_t H5Tencode(hid_tobj_id, unsigned char *buf, size_t *nalloc)
foreign import ccall "hdf5.h H5Tencode"
        c_H5Tencode :: H5Handle -> CString -> Ptr CULLong -> IO CInt



-- hid_t H5Tenum_create(hid_t parent_id )
foreign import ccall "hdf5.h H5Tenum_create"
        c_H5Tenum_create :: H5Handle -> IO H5Handle



-- herr_t H5Tenum_insert( hid_t dtype_id, const char *name, void *value )
foreign import ccall "hdf5.h H5Tenum_insert"
        c_H5Tenum_insert :: H5Handle -> CString -> Ptr () -> IO CInt



-- herr_t H5Tenum_nameof( hid_t dtype_id, void *value, char *name, size_t size )
foreign import ccall "hdf5.h H5Tenum_nameof"
        c_H5Tenum_nameof :: H5Handle -> Ptr () -> CString -> CULLong -> IO CInt



-- herr_t H5Tenum_valueof( hid_t dtype_id, char *name, void *value )
foreign import ccall "hdf5.h H5Tenum_valueof"
        c_H5Tenum_valueof :: H5Handle -> CString -> Ptr () -> IO CInt



-- htri_t H5Tequal( hid_t dtype_id1, hid_t dtype_id2 )
foreign import ccall "hdf5.h H5Tequal"
        c_H5Tequal :: H5Handle -> H5Handle -> IO CInt


{--
-- H5T_conv_t H5Tfind(hid_t src_id, hid_t dst_id, H5T_cdata_t **pcdata )
foreign import ccall "hdf5.h H5Tfind"
        c_H5Tfind :: H5Handle -> H5Handle -> Ptr (Ptr H5T_cdata_t) -> IO H5T_conv_t
--}


-- int H5Tget_array_dims1( hid_t adtype_id, hsize_t dims[], int perm[] )
foreign import ccall "hdf5.h H5Tget_array_dims1"
        c_H5Tget_array_dims1 :: H5Handle -> CULLong -> CInt -> IO CInt



-- int H5Tget_array_dims2( hid_t adtype_id, hsize_t dims[] )
foreign import ccall "hdf5.h H5Tget_array_dims2"
        c_H5Tget_array_dims2 :: H5Handle -> CULLong -> IO CInt



-- int H5Tget_array_ndims( hid_t adtype_id )
foreign import ccall "hdf5.h H5Tget_array_ndims"
        c_H5Tget_array_ndims :: H5Handle -> IO CInt



-- H5T_class_t H5Tget_class( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_class"
        c_H5Tget_class :: H5Handle -> IO H5T_class_t



-- hid_t H5Tget_create_plist( hid_t dtype_id ) 
foreign import ccall "hdf5.h H5Tget_create_plist"
        c_H5Tget_create_plist :: H5Handle -> IO H5Handle



-- H5T_cset_t H5Tget_cset( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_cset"
        c_H5Tget_cset :: H5Handle -> IO H5T_cset_t



-- size_t H5Tget_ebias( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_ebias"
        c_H5Tget_ebias :: H5Handle -> IO CULLong



-- herr_t H5Tget_fields( hid_t dtype_id, size_t *spos, size_t *epos, size_t *esize, size_t *mpos, size_t *msize )
foreign import ccall "hdf5.h H5Tget_fields"
        c_H5Tget_fields :: H5Handle -> Ptr CULLong -> Ptr CULLong -> Ptr CULLong -> Ptr CULLong -> Ptr CULLong -> IO CInt



-- H5T_pad_t H5Tget_inpad( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_inpad"
        c_H5Tget_inpad :: H5Handle -> IO H5T_pad_t



-- H5T_class_t H5Tget_member_class( hid_t cdtype_id, unsigned member_no )
foreign import ccall "hdf5.h H5Tget_member_class"
        c_H5Tget_member_class :: H5Handle -> CInt -> IO H5T_class_t



-- int H5Tget_member_index( hid_t dtype_id, const char * field_name )
foreign import ccall "hdf5.h H5Tget_member_index"
        c_H5Tget_member_index :: H5Handle -> CString -> IO CInt



-- char * H5Tget_member_name( hid_t dtype_id, unsigned field_idx )
foreign import ccall "hdf5.h H5Tget_member_name"
        c_H5Tget_member_name :: H5Handle -> CInt -> IO CString



-- size_t H5Tget_member_offset( hid_t dtype_id, unsigned memb_no )
foreign import ccall "hdf5.h H5Tget_member_offset"
        c_H5Tget_member_offset :: H5Handle -> CInt -> IO CULLong



-- hid_t H5Tget_member_type( hid_t dtype_id, unsigned field_idx )
foreign import ccall "hdf5.h H5Tget_member_type"
        c_H5Tget_member_type :: H5Handle -> CInt -> IO H5Handle



-- herr_t H5Tget_member_value( hid_t dtype_idunsigned memb_no, void *value )
foreign import ccall "hdf5.h H5Tget_member_value"
        c_H5Tget_member_value :: H5Handle -> CInt -> Ptr () -> IO CInt



-- hid_t H5Tget_native_type( hid_t dtype_id, H5T_direction_t direction )
foreign import ccall "hdf5.h H5Tget_native_type"
        c_H5Tget_native_type :: H5Handle -> H5T_direction_t -> IO H5Handle



-- int H5Tget_nmembers( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_nmembers"
        c_H5Tget_nmembers :: H5Handle -> IO CInt



-- H5T_norm_t H5Tget_norm( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_norm"
        c_H5Tget_norm :: H5Handle -> IO H5T_norm_t



-- int H5Tget_offset( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_offset"
        c_H5Tget_offset :: H5Handle -> IO CInt



-- H5T_order_t H5Tget_order( hid_t dtype_id ) 
foreign import ccall "hdf5.h H5Tget_order"
        c_H5Tget_order :: H5Handle -> IO H5T_order_t



-- herr_t H5Tget_pad( hid_t dtype_id, H5T_pad_t * lsb, H5T_pad_t * msb )
foreign import ccall "hdf5.h H5Tget_pad"
        c_H5Tget_pad :: H5Handle -> Ptr H5T_pad_t -> Ptr H5T_pad_t -> IO CInt



-- size_t H5Tget_precision( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_precision"
        c_H5Tget_precision :: H5Handle -> IO CULLong



-- H5T_sign_t H5Tget_sign( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_sign"
        c_H5Tget_sign :: H5Handle -> IO H5T_sign_t



-- size_t H5Tget_size( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_size"
        c_H5Tget_size :: H5Handle -> IO CULLong



-- H5T_str_t H5Tget_strpad( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_strpad"
        c_H5Tget_strpad :: H5Handle -> IO H5T_str_t



-- hid_t H5Tget_super( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_super"
        c_H5Tget_super :: H5Handle -> IO H5Handle



-- char *H5Tget_tag( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tget_tag"
        c_H5Tget_tag :: H5Handle -> IO CString



-- herr_t H5Tinsert( hid_t dtype_id, const char * name, size_t offset, hid_t field_id )
foreign import ccall "hdf5.h H5Tinsert"
        c_H5Tinsert :: H5Handle -> CString -> CULLong -> H5Handle -> IO CInt



-- htri_t H5Tis_variable_str( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tis_variable_str"
        c_H5Tis_variable_str :: H5Handle -> IO CInt



-- herr_t H5Tlock( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tlock"
        c_H5Tlock :: H5Handle -> IO CInt



---- hid_t H5Topen( hid_t loc_id, const char * name ) 
--foreign import ccall "hdf5.h H5Topen"
--        c_H5Topen :: H5Handle -> CString -> IO H5Handle
-- 
-- 
-- 
---- hid_t H5Topen( hid_t loc_id, const char * name, hid_t tapl_id ) 
--foreign import ccall "hdf5.h H5Topen"
--        c_H5Topen :: H5Handle -> CString -> H5Handle -> IO H5Handle



-- hid_t H5Topen1( hid_t loc_id, const char * name )
foreign import ccall "hdf5.h H5Topen1"
        c_H5Topen :: H5Handle -> CString -> IO H5Handle
        --c_H5Topen1 :: H5Handle -> CString -> IO H5Handle



-- hid_t H5Topen2( hid_t loc_id, const char * name, hid_t tapl_id )
foreign import ccall "hdf5.h H5Topen2"
        c_H5Topen2 :: H5Handle -> CString -> H5Handle -> IO H5Handle



-- herr_t H5Tpack( hid_t dtype_id )
foreign import ccall "hdf5.h H5Tpack"
        c_H5Tpack :: H5Handle -> IO CInt


{--
-- herr_t H5Tregister( H5T_pers_t pers, const char * name, hid_t src_id, hid_t dst_id, H5T_conv_t func )
foreign import ccall "hdf5.h H5Tregister"
        c_H5Tregister :: H5T_pers_t -> CString -> H5Handle -> H5Handle -> H5T_conv_t -> IO CInt
--}


-- herr_t H5Tset_cset( hid_t dtype_id, H5T_cset_t cset )
foreign import ccall "hdf5.h H5Tset_cset"
        c_H5Tset_cset :: H5Handle -> H5T_cset_t -> IO CInt



-- herr_t H5Tset_ebias( hid_t dtype_id, size_t ebias )
foreign import ccall "hdf5.h H5Tset_ebias"
        c_H5Tset_ebias :: H5Handle -> CULLong -> IO CInt



-- herr_t H5Tset_fields( hid_t dtype_id, size_t spos, size_t epos, size_t esize, size_t mpos, size_t msize )
foreign import ccall "hdf5.h H5Tset_fields"
        c_H5Tset_fields :: H5Handle -> CULLong -> CULLong -> CULLong -> CULLong -> CULLong -> IO CInt



-- herr_t H5Tset_inpad( hid_t dtype_id, H5T_pad_t inpad )
foreign import ccall "hdf5.h H5Tset_inpad"
        c_H5Tset_inpad :: H5Handle -> H5T_pad_t -> IO CInt



-- herr_t H5Tset_norm( hid_t dtype_id, H5T_norm_t norm )
foreign import ccall "hdf5.h H5Tset_norm"
        c_H5Tset_norm :: H5Handle -> H5T_norm_t -> IO CInt



-- herr_t H5Tset_offset( hid_t dtype_id, size_t offset )
foreign import ccall "hdf5.h H5Tset_offset"
        c_H5Tset_offset :: H5Handle -> CULLong -> IO CInt



-- herr_t H5Tset_order( hid_t dtype_id, H5T_order_t order ) 
foreign import ccall "hdf5.h H5Tset_order"
        c_H5Tset_order :: H5Handle -> H5T_order_t -> IO CInt



-- herr_t H5Tset_pad( hid_t dtype_id, H5T_pad_t lsb, H5T_pad_t msb )
foreign import ccall "hdf5.h H5Tset_pad"
        c_H5Tset_pad :: H5Handle -> H5T_pad_t -> H5T_pad_t -> IO CInt



-- herr_t H5Tset_precision( hid_t dtype_id, size_tprecision )
foreign import ccall "hdf5.h H5Tset_precision"
        c_H5Tset_precision :: H5Handle -> CULLong -> IO CInt



-- herr_t H5Tset_sign( hid_t dtype_id, H5T_sign_t sign )
foreign import ccall "hdf5.h H5Tset_sign"
        c_H5Tset_sign :: H5Handle -> H5T_sign_t -> IO CInt



-- herr_t H5Tset_size( hid_t dtype_id, size_tsize )
foreign import ccall "hdf5.h H5Tset_size"
        c_H5Tset_size :: H5Handle -> CULLong -> IO CInt



-- herr_t H5Tset_strpad( hid_t dtype_id, H5T_str_t strpad )
foreign import ccall "hdf5.h H5Tset_strpad"
        c_H5Tset_strpad :: H5Handle -> H5T_str_t -> IO CInt



-- herr_t H5Tset_tag( hid_t dtype_idconst char *tag )
foreign import ccall "hdf5.h H5Tset_tag"
        c_H5Tset_tag :: H5Handle -> CChar -> IO CInt


{--
-- herr_t H5Tunregister( H5T_pers_t pers, const char *name, hid_t src_id, hid_t dst_id, H5T_conv_t func )
foreign import ccall "hdf5.h H5Tunregister"
        c_H5Tunregister :: H5T_pers_t -> CChar -> H5Handle -> H5Handle -> H5T_conv_t -> IO CInt
--}



-- hid_t H5Tvlen_create( hid_t base_type_id )
foreign import ccall "hdf5.h H5Tvlen_create"
        c_H5Tvlen_create :: H5Handle -> IO H5Handle



