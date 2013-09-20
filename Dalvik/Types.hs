module Dalvik.Types where

import Data.Set (Set)
import Data.Map (Map)
import Data.Word (Word8, Word16, Word32, Word64)

import Dalvik.AccessFlags (AccessFlags)
import Dalvik.RawTypes (CodeItem)

-- http://source.android.com/devices/tech/dalvik/dex-format.html
-- TODO: string syntax

newtype Type = Type String
type Interface = Class

type Code = CodeItem

data Annotation = Annotation { annotationType :: Type
                             , annotationElements :: Map String Value}

data Value = VByte Word8
           | VShort Word16
           | VChar Char
           | VInt Word32
           | VLong Word64
           | VFloat Float
           | VDouble Double
           | VString String
           | VType Type
           | VField Field
           | VMethod Method
           | VEnum Field
           | VArray [Value]
           | VAnnotation Annotation
           | VNull
           | VBoolean Bool
            

data Field = Field { fieldAccessFlags :: AccessFlags
                   , fieldClass :: Class
                   , fieldType :: Type
                   , fieldName :: String
                   , fieldValue :: Maybe Value
                   }

data Method = Method { methodAccessFlags :: AccessFlags
                     , methodShorty :: String
                     , methodReturnType :: Type
                     , methodParameters :: [Type]
                     , methodName :: String
                     , code :: Code
                     }

data Class = Class { classType :: Type
                   , classAccessFlags :: AccessFlags
                   , superClass :: Maybe Class
                   , classInterfaces :: [Interface]
                   , sourceFile :: Maybe String
                   , classAnnotations :: Maybe Annotation
                   , staticFields :: [Field] 
                   , instanceFields :: [Field]
                   , directMethods :: [Method]
                   , virtualMethods :: [Method]
                   }

