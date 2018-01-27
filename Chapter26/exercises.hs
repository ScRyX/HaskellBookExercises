module ChapterExercises where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Functor.Identity

--------
-- 1) --
--------
--rDec :: Num a => ReaderT a Identity a 
--rDec = ReaderT $ (\r -> Identity $ r - 1)
--------
-- 2) --
--------
rDec :: (Num a, Enum a) => ReaderT a Identity a 
rDec = ReaderT $ Identity . pred 
------------
-- 3), 4) --
------------
rShow :: Show a
      => ReaderT a Identity String
rShow = ReaderT $ Identity . show
--------
-- 5) --
--------
rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT $ 
  \r -> do 
    print $ "Hi: " ++ (show r) 
    return (r + 1)
--------
-- 6) --
--------
sPrintIntAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIntAccum = StateT $
  \s -> do
    print $ "Hi: " ++ (show s) 
    return (show s, s + 1)
    
------------------
-- FIX THE CODE --
------------------
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift $ getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of 
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn
        ("Good, was very excite: " ++ e)
