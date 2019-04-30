{-# LANGUAGE ScopedTypeVariables   #-}

module Lib
    ( someFunc
    ) where

import Control.Monad.IO.Class
import Control.Exception.Safe
import Data.Text hiding (take)
import Prelude hiding (read, write)
import System.Environment

data FileException = FileNotFoundException String
                   | AccessDeniedException String
                   | HardwareException     String
                   deriving (Show, Typeable)
 
instance Exception FileException

someFunc :: IO ()
someFunc = do
    tryFile `catch` (\(e::FileException ) -> 
        case e of
            FileNotFoundException msg -> print "File missing"
            AccessDeniedException msg -> print "Insufficient permission"
            HardwareException msg -> print "HDD failure"
        )
    return ()

tryFile :: IO ()
tryFile = do
    -- Compose methods and escape ugly Left Right case patterns
    args <- getArgs
    print args
    content <- pack <$> read (args!!0)
    print $ "File content: " ++ show content
    write (args!!1) $ unpack content
    print "File written"
    sampleThrow False 
    print "No exceptions"
    sampleThrow True
    print "Completed"

read :: (MonadThrow m, MonadIO m) => String -> m String
read filepath = do
    content <- liftIO $ readFile filepath
    return content

write :: (MonadThrow m, MonadIO m) => String -> String -> m ()
write filepath content = liftIO $ writeFile filepath content

sampleThrow :: (MonadThrow m, MonadIO m) => Bool -> m String
sampleThrow bool =
    case bool of
        True -> return "Any type" -- :: a
        False -> throwM $ FileNotFoundException "File missing"
