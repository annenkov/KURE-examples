import Prelude hiding (id)
import Data.List
import Control.Arrow
import Language.KURE
import Language.KURE.Utilities

-- Source AST
data Model = Model { modelName  :: String
              , modelAttrs :: [Attr] }

data Tag = PK | Required
           deriving (Show, Eq)

data Attr = Attr { attrName :: String
                 , attrType :: AttrType
                 , tags     :: [Tag] }
            deriving Show

data AttrType = AttrInteger | AttrString | AttrFloat
                deriving Show

-- Target AST

data Table = Table { tableName :: String
                   , columns :: [Column] }

instance Show Table where
    show (Table name cols) = "CREATE TABLE " ++ name ++ "(" ++ (concat $ intersperse ", " $ map show cols) ++ ")"

data Column = Column { columnName :: String
                     , columnType :: SQLType 
                     , constraints :: [SQLConstraint]
                     , nullAllowed :: Bool }

instance Show Column where
    show (Column name colType constraints nullAllowed) = name ++ " " ++ show colType

data SQLType = SQLInteger | SQLVarchar | SQLReal

instance Show SQLType where
    show SQLInteger = "integer"
    show SQLVarchar = "varchar"
    show SQLReal = "real"

data SQLConstraint = PKConstraint { name :: String }                

type TranslateE a b = Translate () KureMonad a b
type TranslateAttr b = Translate () KureMonad Attr b
type TranslateModel b = Translate () KureMonad Model b

idR = translate $ \ _ -> return

applyE :: TranslateE a b -> a -> Either String b
applyE t = runKureMonad Right Left . apply t ()

attrT = translate $ \_ (Attr name attrType tags)-> do
          colType <- case attrType of
                       AttrInteger -> return SQLInteger
                       AttrString -> return SQLVarchar
                       _ -> fail $ "Translate attribute " ++ name ++ " failed. No SQL equivalent for type: " ++ show attrType
          constraints <- mapM (apply constraintT ()) tags
          return $ Column name colType constraints False
 

--pkT :: TranslateAttr Column
pkT col = translate $ \_ (Attr name attrType tags) -> if (PK `elem` tags) then return col {nullAllowed = True}
                                                       else  fail "" 

translateAttr :: TranslateAttr Column
translateAttr  = do
  Attr name attrType tags <- idR
  colType <- case attrType of
               AttrInteger -> return SQLInteger
               AttrString -> return SQLVarchar
               _ -> fail $ "Translate attribute " ++ name ++ " failed. No SQL equivalent for type: " ++ show attrType
  return $ Column name colType [] False

constraintT = \_ tag -> case tag of
                        PK -> return PKConstraint
                        _ -> fail $ "No rule for tag: " ++ show tag

translateModel :: TranslateModel Table
translateModel = translate $ \_ (Model name attrs) -> do
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

sampleAttrs = [Attr "name" AttrString, Attr "age" AttrFloat]

sampleModel = Model {modelName = "Employee", 
                     modelAttrs = [Attr "id" AttrInteger [], 
                                   Attr "name" AttrString [], 
                                   Attr "age" AttrInteger []]}
trAttrs = do
  cols <- mapT translateAttr
  return cols