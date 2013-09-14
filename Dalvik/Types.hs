module Dalvik.Types where

import Data.Set

import Dalvik.AccessFlags (AccessFlags)
import Dalvik.Types (CodeItem)

-- http://source.android.com/devices/tech/dalvik/dex-format.html

newtype Type = Type String
type Interface = Class

type Annotation = String --XXX
type Code = CodeItem

data Value = VByte Int -- XXX
           | VShort Int
           | VChar Char
           | VInt Int
           | VLong Int
           | VFloat Float
           | VDouble Float
           | VString String
           | VType Type
           | VField Field
           | VMethod Method
           | VEnum Field
           | VArray [Value]
           | VAnnotation Annotation
           | VNull
           | VBoolean Boolean
            

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
