{-# LANGUAGE NamedFieldPuns #-}
module Lib
  ( Connection(..)
  , toggleConnection
  , listConnections
  )
where

import           System.Process
import           Data.List.Split

data Connection =  Connection { _uuid :: String, _cType :: String, _name:: String, _timestamp:: String, _active:: Bool } deriving (Show, Eq)

listConnections :: IO [Connection]
listConnections = toConnections <$> execReadConnections
 where
  execReadConnections = readProcess
    "nmcli"
    [ "--terse"
    , "--colors=no"
    , "--escape=yes"
    , "--fields=uuid,type,name,timestamp,active"
    , "connection"
    , "show"
    ]
    []

toConnections :: String -> [Connection]
toConnections xs = toConnection <$> lines xs

toConnection :: String -> Connection
toConnection x = Connection uuid cType name ts (isActive active)
 where
  [uuid, cType, name, ts, active] = splitOn ":" x
  isActive "yes" = True
  isActive _     = False

toggleConnection :: Connection -> IO String
toggleConnection Connection { _uuid, _active } = readProcess
  "nmcli"
  ["connection", if _active then "down" else "up", _uuid]
  []
