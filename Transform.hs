import Prelude hiding (id)
import Data.List
import Language.KURE
import Language.KURE.Utilities

-- Source AST
data Model = Model { modelName  :: String
              , modelAttrs :: [Attr] }

data Attr = Attr { attrName :: String
                 , attrType :: AttrType
                 , tags     :: [Tag] }
            deriving Show

data AttrType = AttrInteger | AttrString | AttrFloat
                deriving Show

data Tag = PK | Required
           deriving Show

-- Target AST

data Table = Table { tableName :: String
                   , columns :: [Column] }

instance Show Table where
    show (Table name cols) = "CREATE TABLE " ++ name ++ "(" ++ (concat $ intersperse ", " $ map show cols) ++ ")"

data Column = Column { columnName :: String
                     , columnType :: SQLType }

instance Show Column where
    show (Column name colType) = name ++ " " ++ show colType

data SQLType = SQLInteger | SQLVarchar | SQLReal

instance Show SQLType where
    show SQLInteger = "integer"
    show SQLVarchar = "varchar"
    show SQLReal = "real"


type TranslateE a b = Translate () KureMonad a b
type TranslateAttr b = Translate () KureMonad Attr b
type TranslateModel b = Translate () KureMonad Model b

idR = translate $ \ _ -> return

applyE :: TranslateE a b -> a -> Either String b
applyE t = runKureMonad Right Left . apply t ()

translateAttr :: TranslateAttr Column
translateAttr  = do
  Attr name attrType _ <- idR
  case attrType of
    AttrInteger -> return $ Column name SQLInteger
    AttrString -> return $ Column name SQLVarchar
    _ -> fail $ "Translate attribute " ++ name ++ " failed. No SQL equivalent for type: " ++ show attrType

translateModel :: TranslateModel Table
translateModel = translate $ \_ (Model name attrs) -> do
                   cols <- apply trAttrs () attrs
                   return $ Table (name ++ "_table") cols
                   
translateAttr' attr =  case attr of
                            Attr name AttrInteger _ -> return $ Column name SQLInteger
                            _ -> fail "translateAttr fails"

sampleAttrs = [Attr "name" AttrString, Attr "age" AttrFloat]

sampleModel = Model {modelName = "Employee", 
                     modelAttrs = [Attr "id" AttrInteger [], 
                                   Attr "name" AttrString [], 
                                   Attr "age" AttrInteger []]}
trAttrs = do
  attrs <- mapT translateAttr
  return attrs