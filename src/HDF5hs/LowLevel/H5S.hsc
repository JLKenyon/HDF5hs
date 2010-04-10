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

module HDF5hs.LowLevel.H5S where

import HDF5hs.LowLevel.H5Types

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Marshal.Array



-- herr_t H5Sclose(hid_t space_id )
foreign import ccall "hdf5.h H5Sclose"
        c_H5Sclose :: H5Handle -> IO CInt



-- hid_t H5Scopy(hid_t space_id )
foreign import ccall "hdf5.h H5Scopy"
        c_H5Scopy :: H5Handle -> IO H5Handle



{--
-- hid_t H5Screate(H5S_class_t type)
foreign import ccall "hdf5.h H5Screate"
        c_H5Screate :: H5S_class_t -> IO H5Handle
--}


-- hid_t H5Screate_simple(int rank, const hsize_t * dims, const hsize_t * maxdims )
foreign import ccall "hdf5.h H5Screate_simple"
        c_H5Screate_simple :: CInt -> Ptr CULLong -> Ptr CULLong -> IO H5Handle



-- hid_t H5Sdecode (unsigned char *buf)
foreign import ccall "hdf5.h H5Sdecode"
        c_H5Sdecode :: Ptr CChar -> IO H5Handle



-- herr_t H5Sencode(hid_tobj_id, unsigned char *buf, size_t *nalloc)
foreign import ccall "hdf5.h H5Sencode"
        c_H5Sencode :: H5Handle -> Ptr CChar -> Ptr CULLong -> IO CInt



-- herr_t H5Sextent_copy(hid_t dest_space_id, hid_t source_space_id )
foreign import ccall "hdf5.h H5Sextent_copy"
        c_H5Sextent_copy :: H5Handle -> H5Handle -> IO CInt



-- htri_t H5Sextent_equal(hid_tspace1_id, hid_t space2_id)
foreign import ccall "hdf5.h H5Sextent_equal"
        c_H5Sextent_equal :: H5Handle -> H5Handle -> IO CInt



-- herr_t H5Sget_select_bounds(hid_t space_id, hsize_t *start, hsize_t *end )
foreign import ccall "hdf5.h H5Sget_select_bounds"
        c_H5Sget_select_bounds :: H5Handle -> Ptr CULLong -> Ptr CULLong -> IO CInt



-- hssize_t H5Sget_select_elem_npoints(hid_t space_id )
foreign import ccall "hdf5.h H5Sget_select_elem_npoints"
        c_H5Sget_select_elem_npoints :: H5Handle -> IO CULLong



-- herr_t H5Sget_select_elem_pointlist(hid_t space_id, hsize_t startpoint, hsize_t numpoints, hsize_t *buf )
foreign import ccall "hdf5.h H5Sget_select_elem_pointlist"
        c_H5Sget_select_elem_pointlist :: H5Handle -> CULLong -> CULLong -> Ptr CULLong -> IO CInt



-- herr_t H5Sget_select_hyper_blocklist(hid_t space_id, hsize_t startblock, hsize_t numblocks, hsize_t *buf )
foreign import ccall "hdf5.h H5Sget_select_hyper_blocklist"
        c_H5Sget_select_hyper_blocklist :: H5Handle -> CULLong -> CULLong -> Ptr CULLong -> IO CInt



-- hssize_t H5Sget_select_hyper_nblocks(hid_t space_id )
foreign import ccall "hdf5.h H5Sget_select_hyper_nblocks"
        c_H5Sget_select_hyper_nblocks :: H5Handle -> IO CULLong



-- hssize_t H5Sget_select_npoints(hid_t space_id)
foreign import ccall "hdf5.h H5Sget_select_npoints"
        c_H5Sget_select_npoints :: H5Handle -> IO CULLong



{--
-- H5S_sel_type H5Sget_select_type(hid_t space_id)
foreign import ccall "hdf5.h H5Sget_select_type"
        c_H5Sget_select_type :: H5Handle -> IO H5S_sel_type
--}


-- int H5Sget_simple_extent_dims(hid_t space_id, hsize_t *dims, hsize_t *maxdims )
foreign import ccall "hdf5.h H5Sget_simple_extent_dims"
        c_H5Sget_simple_extent_dims :: H5Handle -> Ptr CULLong -> Ptr CULLong -> IO CInt



-- int H5Sget_simple_extent_ndims(hid_t space_id)
foreign import ccall "hdf5.h H5Sget_simple_extent_ndims"
        c_H5Sget_simple_extent_ndims :: H5Handle -> IO CInt



-- hssize_t H5Sget_simple_extent_npoints(hid_t space_id)
foreign import ccall "hdf5.h H5Sget_simple_extent_npoints"
        c_H5Sget_simple_extent_npoints :: H5Handle -> IO CULLong


{--
-- H5S_class_t H5Sget_simple_extent_type(hid_t space_id)
foreign import ccall "hdf5.h H5Sget_simple_extent_type"
        c_H5Sget_simple_extent_type :: H5Handle -> IO H5S_class_t
--}


-- htri_t H5Sis_simple(hid_t space_id)
foreign import ccall "hdf5.h H5Sis_simple"
        c_H5Sis_simple :: H5Handle -> IO CInt



-- herr_t H5Soffset_simple(hid_t space_id, const hssize_t *offset )
foreign import ccall "hdf5.h H5Soffset_simple"
        c_H5Soffset_simple :: H5Handle -> Ptr CULLong -> IO CInt



-- herr_t H5Sselect_all( hid_t dspace_id )
foreign import ccall "hdf5.h H5Sselect_all"
        c_H5Sselect_all :: H5Handle -> IO CInt


{--
-- herr_t H5Sselect_elements(hid_t space_id, H5S_seloper_t op, size_t num_elements, const hsize_t *coord )
foreign import ccall "hdf5.h H5Sselect_elements"
        c_H5Sselect_elements :: H5Handle -> H5S_seloper_t -> CULLong -> Ptr CULLong -> IO CInt
--}

{--
-- herr_t H5Sselect_hyperslab(hid_t space_id, H5S_seloper_t op, const hsize_t *start, const hsize_t *stride, const hsize_t *count, const hsize_t *block )
foreign import ccall "hdf5.h H5Sselect_hyperslab"
        c_H5Sselect_hyperslab :: H5Handle -> H5S_seloper_t -> Ptr CULLong -> Ptr CULLong -> Ptr CULLong -> Ptr CULLong -> IO CInt
--}


-- herr_t H5Sselect_none(hid_t space_id)
foreign import ccall "hdf5.h H5Sselect_none"
        c_H5Sselect_none :: H5Handle -> IO CInt



-- htri_t H5Sselect_valid(hid_t space_id)
foreign import ccall "hdf5.h H5Sselect_valid"
        c_H5Sselect_valid :: H5Handle -> IO CInt



-- herr_t H5Sset_extent_none(hid_t space_id)
foreign import ccall "hdf5.h H5Sset_extent_none"
        c_H5Sset_extent_none :: H5Handle -> IO CInt



-- herr_t H5Sset_extent_simple(hid_t space_id, int rank, const hsize_t *current_size, const hsize_t *maximum_size )
foreign import ccall "hdf5.h H5Sset_extent_simple"
        c_H5Sset_extent_simple :: H5Handle -> CInt -> Ptr CULLong -> Ptr CULLong -> IO CInt



