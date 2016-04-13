{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

newtype PatientId = PatientId String
    deriving (Show, Ord, Eq)

newtype PatientName = PatientName String
    deriving (Show)

-- A request does not necessarily is in one to one correspondance with
-- exactly one response.

data KisRequest =
      CreatePatient { patientId :: PatientId, patientName :: PatientName }
    | GetPatients
    | GetPatient PatientId
    | DeletePatient PatientId
    deriving (Show)

data KisResponse =
      CreatedPatient PatientName
    | PatientList [PatientName]
    | Patient (Maybe PatientName)
    | DeletedPatient PatientId
    deriving (Show)

type PatientDict = M.Map PatientId PatientName

type Kis a = StateT PatientDict IO a

main :: IO ()
main = do
    requests <- liftM (map toKisRequest . lines) getContents
    evalStateT (mapM_ process requests) M.empty

process :: KisRequest -> Kis ()
process request = do
    lift $ print request
    res <- action request
    -- notify res
    lift $ print res

action :: KisRequest -> Kis KisResponse
action (CreatePatient id name) = modify (M.insert id name) >> return (CreatedPatient name)
action GetPatients = liftM PatientList (gets M.elems)
action (GetPatient id) = liftM Patient (gets (M.lookup id))
action (DeletePatient id) = modify (M.delete id) >> return (DeletedPatient id)

toKisRequest :: String -> KisRequest
toKisRequest cmd@('c':_) = CreatePatient (idArg cmd) (PatientName name)
    where name = last (cmdArgs cmd)
toKisRequest cmd@('g':_) = GetPatient (idArg cmd)
toKisRequest cmd@('d':_) = DeletePatient (idArg cmd)
toKisRequest _ = GetPatients

cmdArgs :: String -> [String]
cmdArgs = tail . words

idArg :: String -> PatientId
idArg = PatientId . head . cmdArgs

data Create = Create deriving (Show)
data Admit = Admit deriving (Show)
data Delete = Delete deriving (Show)

data Created = Created deriving (Show)
data Admitted = Admitted deriving (Show)
data Deleted = Deleted deriving (Show)

class Request a b | a -> b where
    work :: a -> b
    work = undefined

instance Request Create Created
instance Request Delete Deleted
instance Request Admit Admitted
