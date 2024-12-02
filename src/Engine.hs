module Engine where

class Engine a where
  initialize :: [String] -> IO (Either String a)

  run :: a -> IO ()