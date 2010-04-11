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

module HDF5hs.LowLevel.H5Types where

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

data H5AC_cache_config_t = H5AC_cache_config_t {}
data H5A_info_t          = H5A_info_t          {}
data H5A_operator1_t     = H5A_operator1_t     {}
data H5A_operator2_t     = H5A_operator2_t     {}
data H5D_operator_t      = H5D_operator_t      {}
data H5D_space_status_t  = H5D_space_status_t  {}
data H5F_info_t          = H5F_info_t          {}
data H5F_scope_t         = H5F_scope_t         {}
data H5G_info_t          = H5G_info_t          {}
data H5G_iterate_t       = H5G_iterate_t       {}
data H5G_link_t          = H5G_link_t          {}
data H5G_stat_t          = H5G_stat_t          {}
data H5_index_t          = H5_index_t          {}
data H5_iter_order_t     = H5_iter_order_t     {}
data H5L_class_t         = H5L_class_t         {}
data H5L_info_t          = H5L_info_t          {}
data H5L_iterate_t       = H5L_iterate_t       {}
data H5LT_lang_t         = H5LT_lang_t         {}
data H5L_type_t          = H5L_type_t          {}



newtype H5T_pad_t  = H5T_pad_t  { unH5T_pad_t  :: CInt }
    deriving (Eq,Show)

newtype H5T_pers_t = H5T_pers_t { unH5T_pers_t :: CInt }
    deriving (Eq,Show)

newtype H5T_sign_t = H5T_sign_t { unH5T_sign_t :: CInt }
    deriving (Eq,Show)

newtype H5T_str_t  = H5T_str_t  { unH5T_str_t  :: CInt }
    deriving (Eq,Show)

newtype H5S_class_t = H5S_class_t { unH5S_class_t :: CInt }
    deriving (Eq,Show)

newtype H5T_cset_t      = H5T_cset_t      { unH5T_cset_t      :: CInt } 
    deriving (Eq,Show)

newtype H5T_direction_t = H5T_direction_t { unH5T_direction_t :: CInt } 
    deriving (Eq,Show)

newtype H5T_norm_t      = H5T_norm_t      { unH5T_norm_t      :: CInt } 
    deriving (Eq,Show)

newtype H5T_order_t     = H5T_order_t     { unH5T_order_t     :: CInt } 
    deriving (Eq,Show)

#{enum H5T_order_t, H5T_order_t
 , h5T_order_error = H5T_ORDER_ERROR
 , h5T_order_le    = H5T_ORDER_LE   
 , h5T_order_be    = H5T_ORDER_BE   
 , h5T_order_vax   = H5T_ORDER_VAX  
 , h5T_order_none  = H5T_ORDER_NONE 
}

data H5T_cdata_t = H5T_cdata_t {}
data H5T_conv_t  = H5T_conv_t {}

#{enum H5S_class_t, H5S_class_t
 , h5Sno_class = H5S_NO_CLASS
 , h5Sscalar   = H5S_SCALAR  
 , h5Ssimple   = H5S_SIMPLE  
}
-- , h5Scomplex  = H5S_COMPLEX

newtype H5S_seloper_t = H5S_seloper_t { unH5S_seloper_t :: CInt } 
    deriving (Eq,Show)


#{enum H5S_seloper_t, H5S_seloper_t
 , h5Sselect_noop    = H5S_SELECT_NOOP   
 , h5Sselect_set     = H5S_SELECT_SET    
 , h5Sselect_or      = H5S_SELECT_OR     
 , h5Sselect_and     = H5S_SELECT_AND
 , h5Sselect_xor     = H5S_SELECT_XOR
 , h5Sselect_notb    = H5S_SELECT_NOTB
 , h5Sselect_nota    = H5S_SELECT_NOTA
 , h5Sselect_append  = H5S_SELECT_APPEND 
 , h5Sselect_prepend = H5S_SELECT_PREPEND
 , h5Sselect_invalid = H5S_SELECT_INVALID
}

newtype H5S_sel_type  = H5S_sel_type  { unH5S_sel_type  :: CInt } 
    deriving (Eq,Show)

#{enum H5S_sel_type, H5S_sel_type
 , h5Ssel_error      = H5S_SEL_ERROR
 , h5Ssel_none       = H5S_SEL_NONE
 , h5Ssel_points     = H5S_SEL_POINTS
 , h5Ssel_hyperslabs = H5S_SEL_HYPERSLABS 
 , h5Ssel_all        = H5S_SEL_ALL
 , h5Ssel_n          = H5S_SEL_N
}


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

h5Pdefault :: H5Handle
h5Pdefault = H5Handle $ #const H5P_DEFAULT

h5Sall :: H5Handle
h5Sall = H5Handle $ #const H5S_ALL


newtype H5Scope = H5Scope { unH5Scope :: CInt }
    deriving (Eq, Show)

#{enum H5Scope, H5Scope
 , h5Slocal  = H5F_SCOPE_LOCAL
 , h5Sglobal = H5F_SCOPE_GLOBAL
 , h5Sdown   = H5F_SCOPE_DOWN
}

newtype H5Handle = H5Handle {unH5Handle :: CInt }
    deriving (Eq,Show)

#{enum H5Handle, H5Handle
 , h5T_native_char    = H5T_NATIVE_CHAR     
 , h5T_native_schar   = H5T_NATIVE_SCHAR    
 , h5T_native_uchar   = H5T_NATIVE_UCHAR    
 , h5T_native_short   = H5T_NATIVE_SHORT    
 , h5T_native_ushort  = H5T_NATIVE_USHORT   
 , h5T_native_int     = H5T_NATIVE_INT      
 , h5T_native_uint    = H5T_NATIVE_UINT     
 , h5T_native_long    = H5T_NATIVE_LONG     
 , h5T_native_ulong   = H5T_NATIVE_ULONG    
 , h5T_native_llong   = H5T_NATIVE_LLONG    
 , h5T_native_ullong  = H5T_NATIVE_ULLONG   
 , h5T_native_float   = H5T_NATIVE_FLOAT    
 , h5T_native_double  = H5T_NATIVE_DOUBLE   
 , h5T_native_ldouble = H5T_NATIVE_LDOUBLE  
 , h5T_native_b8      = H5T_NATIVE_B8       
 , h5T_native_b16     = H5T_NATIVE_B16      
 , h5T_native_b32     = H5T_NATIVE_B32      
 , h5T_native_b64     = H5T_NATIVE_B64      
 , h5T_native_opaque  = H5T_NATIVE_OPAQUE   
 , h5T_native_haddr   = H5T_NATIVE_HADDR    
 , h5T_native_hsize   = H5T_NATIVE_HSIZE    
 , h5T_native_hssize  = H5T_NATIVE_HSSIZE   
 , h5T_native_herr    = H5T_NATIVE_HERR     
 , h5T_native_hbool   = H5T_NATIVE_HBOOL    
}

newtype H5T_class_t = H5T_class_t { unH5T_class_t :: CInt }
    deriving (Eq, Show)

