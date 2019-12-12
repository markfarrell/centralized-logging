module Windows
  ( Entry
  ) where

import Prelude

import Data.Traversable(foldMap)
import Data.String.CodeUnits (singleton)
import Data.List( many)

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (string, satisfy)

newtype Entry = Entry
  { eventID :: String
  , machineName :: String
  , entryData :: String
  , entryIndex :: String
  , category :: String
  , categoryNumber :: String
  , entryType :: String
  , message :: String
  , source :: String
  , replacementStrings :: String
  , instanceID :: String
  , timeGenerated :: String
  , timeWritten :: String
  , userName :: String
  , site :: String
  , container :: String
  } 

parseValue :: Parser String String
parseValue = foldMap singleton <$> many (satisfy $ not <<< eq ',')

parseEntry :: Parser String Entry
parseEntry = do
  eventID        <- parseValue
  _              <- string ","
  machineName    <- parseValue
  _              <- string ","
  entryData      <- parseValue
  _              <- string ","
  entryIndex     <- parseValue
  _              <- string ","
  category       <- parseValue 
  _              <- string ","
  categoryNumber <- parseValue
  _              <- string ","
  entryType      <- parseValue
  _              <- string ","
  message        <- parseValue
  _              <- string ","
  source         <- parseValue
  _              <- string ","
  replacementStr <- parseValue
  _              <- string ","
  instanceID     <- parseValue
  _              <- string ","
  timeGenerated  <- parseValue
  _              <- string "," 
  timeWritten    <- parseValue
  _              <- string ","
  userName       <- parseValue
  _              <- string ","
  site           <- parseValue
  _              <- string ","
  container      <- parseValue
  pure $ Entry
    { eventID : eventID
    , machineName : machineName
    , entryData : entryData
    , entryIndex : entryIndex
    , category : category
    , categoryNumber : categoryNumber
    , entryType : entryType
    , message : message
    , source : source
    , replacementStrings : replacementStr
    , instanceID : instanceID
    , timeGenerated : timeGenerated
    , timeWritten : timeWritten
    , userName : userName
    , site : site
    , container : container
    }
  
