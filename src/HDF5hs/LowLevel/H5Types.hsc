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




#include <hdf5.h>

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

newtype H5Scope = H5Scope { unH5Scope :: CInt }
    deriving (Eq, Show)

#{enum H5Scope, H5Scope
 , h5Slocal  = H5F_SCOPE_LOCAL
 , h5Sglobal = H5F_SCOPE_GLOBAL
 , h5Sdown   = H5F_SCOPE_DOWN
}

newtype H5Handle = H5Handle {unH5Handle :: CInt }
    deriving (Eq,Show)

