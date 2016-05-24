{-# LANGUAGE TemplateHaskell #-}
module Kis.Notifications
    ( NotificationType(..)
    )
where

import Database.Persist.TH

-- TODO Find a way to add richer information into the Notifications.
-- Probably we will need multiple tables in the DB.
data NotificationType = NewPatient | NewBed | PatientMoved | NoNotif
    deriving (Show, Read, Eq)

derivePersistField "NotificationType"
