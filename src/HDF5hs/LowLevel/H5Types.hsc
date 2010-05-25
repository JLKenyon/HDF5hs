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

#{enum H5T_pad_t, H5T_pad_t
 , h5T_pad_error      = H5T_PAD_ERROR     
 , h5T_pad_zero       = H5T_PAD_ZERO      
 , h5T_pad_one        = H5T_PAD_ONE       
 , h5T_pad_background = H5T_PAD_BACKGROUND
 , h5T_npad           = H5T_NPAD          
}

newtype H5T_pers_t = H5T_pers_t { unH5T_pers_t :: CInt }
    deriving (Eq,Show)

#{enum H5T_pers_t, H5T_pers_t
 , h5t_pers_dontcare = H5T_PERS_DONTCARE
 , h5t_pers_hard     = H5T_PERS_HARD
 , h5t_pers_soft     = H5T_PERS_SOFT
}

newtype H5T_sign_t = H5T_sign_t { unH5T_sign_t :: CInt }
    deriving (Eq,Show)

#{enum H5T_sign_t, H5T_sign_t
 , h5T_sgn_error = H5T_SGN_ERROR
 , h5T_sgn_none  = H5T_SGN_NONE
 , h5T_sgn_2     = H5T_SGN_2
 , h5T_nsgn      = H5T_NSGN
}

newtype H5T_str_t  = H5T_str_t  { unH5T_str_t  :: CInt }
    deriving (Eq,Show)

#{enum H5T_str_t, H5T_str_t
 , h5T_str_error    = H5T_STR_ERROR   
 , h5T_str_nullterm = H5T_STR_NULLTERM
 , h5T_str_nullpad  = H5T_STR_NULLPAD 
 , h5T_str_spacepad = H5T_STR_SPACEPAD
}

newtype H5S_class_t = H5S_class_t { unH5S_class_t :: CInt }
    deriving (Eq,Show)

newtype H5T_cset_t      = H5T_cset_t      { unH5T_cset_t      :: CInt } 
    deriving (Eq,Show)

#{enum H5T_cset_t, H5T_cset_t
 , h5T_cset_error = H5T_CSET_ERROR
 , h5T_cset_ascii = H5T_CSET_ASCII
 , h5T_cset_utf8  = H5T_CSET_UTF8 
}

newtype H5T_direction_t = H5T_direction_t { unH5T_direction_t :: CInt } 
    deriving (Eq,Show)

#{enum H5T_direction_t, H5T_direction_t
 , h5T_dir_default = H5T_DIR_DEFAULT
 , h5T_dir_ascend  = H5T_DIR_ASCEND 
 , h5T_dir_descend = H5T_DIR_DESCEND
}

newtype H5T_norm_t      = H5T_norm_t      { unH5T_norm_t      :: CInt } 
    deriving (Eq,Show)

#{enum H5T_norm_t, H5T_norm_t
 , h5T_norm_error   = H5T_NORM_ERROR
 , h5T_norm_implied = H5T_NORM_IMPLIED
 , h5T_norm_msbset  = H5T_NORM_MSBSET
 , h5T_norm_none    = H5T_NORM_NONE
}

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
 , h5S_no_class = H5S_NO_CLASS
 , h5S_scalar   = H5S_SCALAR  
 , h5S_simple   = H5S_SIMPLE  
}
-- , h5Scomplex  = H5S_COMPLEX

newtype H5S_seloper_t = H5S_seloper_t { unH5S_seloper_t :: CInt } 
    deriving (Eq,Show)

#{enum H5S_seloper_t, H5S_seloper_t
 , h5S_select_noop    = H5S_SELECT_NOOP   
 , h5S_select_set     = H5S_SELECT_SET    
 , h5S_select_or      = H5S_SELECT_OR     
 , h5S_select_and     = H5S_SELECT_AND
 , h5S_select_xor     = H5S_SELECT_XOR
 , h5S_select_notb    = H5S_SELECT_NOTB
 , h5S_select_nota    = H5S_SELECT_NOTA
 , h5S_select_append  = H5S_SELECT_APPEND 
 , h5S_select_prepend = H5S_SELECT_PREPEND
 , h5S_select_invalid = H5S_SELECT_INVALID
}

newtype H5S_sel_type  = H5S_sel_type  { unH5S_sel_type  :: CInt } 
    deriving (Eq,Show)

#{enum H5S_sel_type, H5S_sel_type
 , h5S_sel_error      = H5S_SEL_ERROR
 , h5S_sel_none       = H5S_SEL_NONE
 , h5S_sel_points     = H5S_SEL_POINTS
 , h5S_sel_hyperslabs = H5S_SEL_HYPERSLABS 
 , h5S_sel_all        = H5S_SEL_ALL
 , h5S_sel_n          = H5S_SEL_N
}

newtype H5OpenFlag = H5OpenFlag { unH5OpenFlag  :: CInt }
    deriving (Eq,Show)

#{enum H5OpenFlag, H5OpenFlag
 , h5F_overwrite = H5F_ACC_TRUNC
 , h5F_abort     = H5F_ACC_EXCL
 }

newtype H5OpenMode = H5OpenMode { unH5OpenMode  :: CInt }
    deriving (Eq,Show)

#{enum H5OpenMode, H5OpenMode
 , h5F_readonly  = H5F_ACC_RDONLY
 , h5F_readwrite = H5F_ACC_RDWR
 }

newtype H5TypeClass = H5TypeClass { unH5TypeClass :: CInt }
    deriving (Eq, Show, Storable)

#{enum H5TypeClass, H5TypeClass
 , h5F_no_class  = H5T_NO_CLASS 
 , h5F_integer   = H5T_INTEGER  
 , h5F_float     = H5T_FLOAT    
 , h5F_time      = H5T_TIME     
 , h5F_string    = H5T_STRING   
 , h5F_bitfield  = H5T_BITFIELD 
 , h5F_opaque    = H5T_OPAQUE   
 , h5F_compound  = H5T_COMPOUND 
 , h5F_reference = H5T_REFERENCE
 , h5F_enum      = H5T_ENUM     
 , h5F_vlen      = H5T_VLEN     
 , h5F_array     = H5T_ARRAY    
 , h5F_nclasses  = H5T_NCLASSES 
 }
 
h5F_default :: CInt
h5F_default = #const H5P_DEFAULT

h5P_default :: H5Handle
h5P_default = H5Handle $ #const H5P_DEFAULT

h5S_all :: H5Handle
h5S_all = H5Handle $ #const H5S_ALL

newtype H5Scope = H5Scope { unH5Scope :: CInt }
    deriving (Eq, Show)

#{enum H5Scope, H5Scope
 , h5S_local  = H5F_SCOPE_LOCAL
 , h5S_global = H5F_SCOPE_GLOBAL
}

-- , h5S_down   = H5F_SCOPE_DOWN

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

#{enum H5T_class_t, H5T_class_t
 , h5T_no_class       = H5T_NO_CLASS 
 , h5T_integer        = H5T_INTEGER  
 , h5T_float          = H5T_FLOAT    
 , h5T_time           = H5T_TIME     
 , h5T_string         = H5T_STRING   
 , h5T_bitfield       = H5T_BITFIELD 
 , h5T_opaque         = H5T_OPAQUE   
 , h5T_compound       = H5T_COMPOUND 
 , h5T_reference      = H5T_REFERENCE
 , h5T_enum           = H5T_ENUM     
 , h5T_vlen           = H5T_VLEN     
 , h5T_array          = H5T_ARRAY    
}

