import Prelude hiding (id)
import Data.List
import Language.KURE
import Language.KURE.Utilities

data AttrType = AttrInteger | AttrString | AttrFloat
                deriving Show

data Tag = PK | Required
           deriving Show

data Attr = Attr { attrName :: String
                 , attrType :: AttrType
                 , tags :: [Tag] }
            deriving Show

data Model = Model { modelName  :: String
                   , modelAttrs :: [Attr] }

data SQLType = SQLInteger | SQLVarchar | SQLReal

instance Show SQLType where
    show SQLInteger = "integer"
    show SQLVarchar = "varchar"

data Column = Column { columnName :: String
                     , columnType :: SQLType }

instance Show Column where
    show (Column name colType) = name ++ " " ++ show colType

data Table = Table { tableName :: String
                   , columns :: [Column] }

instance Show Table where
    show (Table name cols) = "CREATE TABLE " ++ name ++ "(" ++ (concat $ intersperse ", " $ map show cols) ++ ")"

type TranslateE a b = Translate () KureMonad a b
type TranslateAttr b = Translate () KureMonad Attr b
type TranslateClass b = Translate () KureMonad Model b

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

translateClass :: TranslateClass Table
translateClass = translate $ \_ (Model name attrs) -> do
                   cols <- apply trAttrs () attrs
                   return $ Table (name ++ "_table") cols
                   
--translateClass = do
--  Class name attrs <- idR
--  columns <- trAttrs
--  return $ Table (name ++ "_table") []

--translateAttrType :: Attr -> Column
translateAttr' attr =  case attr of
                            Attr name AttrInteger _ -> return $ Column name SQLInteger
                            _ -> fail "translateAttr fails"

sample_attrs = [Attr "name" AttrString, Attr "age" AttrFloat]

--trAttrs :: Translate () KureMonad [Attr] [Column]
trAttrs = do
  attrs <- mapT translateAttr
  return attrs