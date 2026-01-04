module LoxError (ScanError) where

data ScanError = ScanError
  { line :: String,
    message :: String
  }
  deriving (Show)