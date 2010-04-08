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

module HDF5hs.LowLevel.H5LT where

--herr_t H5LTmake_dataset ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, hid_t type_id, const void *buffer ) 


--herr_t H5LTmake_dataset_char ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const char *buffer ) 
--herr_t H5LTmake_dataset_short ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const short *buffer ) 
--herr_t H5LTmake_dataset_int ( hid_t  loc_id, const char *dset_name, int rank, const hsize_t *dims, const int *buffer ) 
--herr_t H5LTmake_dataset_long ( hid_t  loc_id, const char *dset_name, int rank, const hsize_t *dims, const long *buffer ) 
--herr_t H5LTmake_dataset_float ( hid_t  loc_id, const char *dset_name, int rank, const hsize_t *dims, const float *buffer )
--herr_t H5LTmake_dataset_double ( hid_t  loc_id, const char *dset_name, int rank, const hsize_t *dims, const double *buffer )
--herr_t H5LTmake_dataset_string ( hid_t  loc_id, const char *dset_name, const char *buffer ) 
--herr_t H5LTread_dataset ( hid_t  loc_id, const char *dset_name, hid_t  type_id,  void *buffer )
--herr_t H5LTread_dataset_char ( hid_t  loc_id, const char *dset_name, char *buffer )
--herr_t H5LTread_dataset_short ( hid_t  loc_id, const char *dset_name, short *buffer )
--herr_t H5LTread_dataset_int ( hid_t  loc_id, const char *dset_name, int *buffer )
--herr_t H5LTread_dataset_long ( hid_t  loc_id, const char *dset_name, long *buffer )
--herr_t H5LTread_dataset_float ( hid_t  loc_id, const char *dset_name, float *buffer )
--herr_t H5LTread_dataset_double ( hid_t  loc_id, const char *dset_name, double *buffer )
--herr_t H5LTread_dataset_string ( hid_t  loc_id, const char *dset_name, char *buffer )
--herr_t H5LTfind_dataset ( hid_t loc_id, const char *dset_name )
--herr_t H5LTget_dataset_ndims ( hid_t loc_id, const char *dset_name, int *rank )
--herr_t H5LTget_dataset_info ( hid_t loc_id, const char *dset_name, hsize_t *dims, H5T_class_t *class_id, size_t *type_size )
--herr_t H5LTset_attribute_string( hid_t loc_id, const char *obj_name, const char *attr_name, const char *attr_data )
--herr_t H5LTset_attribute_char ( hid_t loc_id, const char *obj_name, const char *attr_name, char *buffer, hsize_t size)
--herr_t H5LTset_attribute_short ( hid_t loc_id, const char *obj_name, const char *attr_name, short *buffer, hsize_t size)
--herr_t H5LTset_attribute_int( hid_t loc_id, const char *obj_name, const char *attr_name, int *buffer, hsize_t size)
--herr_t H5LTset_attribute_long ( hid_t loc_id, const char *obj_name, const char *attr_name, long *buffer, hsize_t size)
--herr_t H5LTset_attribute_long_long (hid_t  loc_id, const char *obj_name, const char *attr_name, const long_long *data, size_t size) 
--herr_t H5LTset_attribute_float( hid_t loc_id, const char *obj_name, const char *attr_name, float *buffer, hsize_t size )
--herr_t H5LTset_attribute_double ( hid_t loc_id, const char *obj_name, const char *attr_name, const double *buffer, size_t size )
--herr_t H5LTget_attribute( hid_t loc_id, const char *obj_name, const char *attr_name,  hid_t mem_type_id, void *data )
--herr_t H5LTget_attribute_string( hid_t loc_id, const char *obj_name, const char *attr_name,  char *data )
--herr_t H5LTget_attribute( hid_t loc_id, const char *obj_name, const char *attr_name,  char *data )
--herr_t H5LTget_attribute_short( hid_t loc_id, const char *obj_name, const char *attr_name,  short *data )
--herr_t H5LTget_attribute_int( hid_t loc_id, const char *obj_name, const char *attr_name,  int *data )
--herr_t H5LTget_attribute_long( hid_t loc_id, const char *obj_name, const char *attr_name,  long *data )
--herr_t H5LTget_attribute_long_long (hid_t  loc_id, const char *obj_name, const char *attr_name, long_long *data) 
--herr_t H5LTget_attribute_float( hid_t loc_id, const char *obj_name, const char *attr_name,  float *data )
--herr_t H5LTget_attribute_double( hid_t loc_id, const char *obj_name, const char *attr_name,  double *data )
--herr_t H5LTget_attribute_ndims( hid_t loc_id, const char *obj_name, const char *attr_name,  int *rank )
--herr_t H5LTget_attribute_info( hid_t loc_id, const char *obj_name, const char *attr_name,  hsize_t *dims, H5T_class_t *type_class, size_t *type_size )
--hid_t H5LTtext_to_dtype( const char *text, H5LT_lang_t lang_type) 
--herr_t H5LTdtype_to_text(hid_t datatype, char* str, H5LT_lang_t lang_type, size_t* len) 











