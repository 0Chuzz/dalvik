module Dalvik.Extract where
import Prelude hiding (lookup)
import Data.Map
import Data.Maybe
import Control.Monad (ap)

import qualified Dalvik.RawTypes as RT
import qualified Dalvik.Types as T
import Dalvik.AccessFlags

classes :: RT.DexFile -> [T.Class]
classes file = do 
        classid <- keys . RT.dexClasses $ file
        ret <- maybeToList $ classItemToClass file classid
        return ret

bsToType :: a -> T.Type
bsToType = undefined

classItemToClass :: RT.DexFile ->  RT.TypeId -> Maybe T.Class
classItemToClass dexfile classId = do
        ctype <-  getClassType dexfile classId
        aflags <- getClassFlags dexfile classId
        sclassid <- getSuperClass dexfile classId
        return T.Class { T.classType = bsToType ctype
                       , T.classAccessFlags = aflags
                       , T.superClass = classItemToClass dexfile sclassid
                       }
    where classItem d i = lookup i $ RT.dexClasses d
          getClassType = RT.getTypeName 
          getClassFlags d i= Just . RT.classAccessFlags =<< classItem d i
          getSuperClass d i= return . RT.classSuperId =<< classItem d i

                  
