-- |
-- Module      :  LogParserPrim
-- Copyright   :  (c) Vitaliy Rukavishnikov 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Log files parsing primitives 

module Database.Sybase.Sysmon.LogParserPrim 
       ( LogState (..)
       , Field
       , matchLine
       , matchField
       , optLine
       , optString
       , optField
       , string
       , field
       , look
       , lookAhead
       , whileJust
       , goto
       ) where
import Data.List
import Data.DateTime
import Control.Monad.State

type LogState a = State [String] a 
type Field = String

-- | Discard until predicate and split a string to the fields
line :: (String -> Bool) -> LogState [Field]
line p = do
   xs <- get
   let ys = dropWhile (not . p)  xs   
   if null ys then do
      put ys
      return []
    else do
      put $ drop 1 ys
      return $ (words . head) ys 

-- | Recursively collect values contained in the Just  
whileJust :: Monad m => m (Maybe a) -> m [a]
whileJust p = do
  x <- p
  case x of
    Nothing -> return mzero
    Just x -> do
      xs <- whileJust p
      return $ return x `mplus` xs

-- | Get field after discarding the prefix 
matchField :: Read a => String -> Int -> LogState a
matchField s i = do
  ls <- line (isInfixOf s)
  return $ field i ls

-- | Get fields after discarding the prefix
matchLine :: String -> LogState [Field]
matchLine s = line (isInfixOf s) 

-- | Parse string to the field value
field :: Read a => Int -> [Field] -> a
field i xs = read $ xs !! i

-- | Concatenate fields to the string value
string :: Int -> Int -> [Field] -> Field
string i j = unwords . take (j - i) . drop i

-- | Look ahead for the first substring until the second substring 
look :: String -> String -> LogState Bool
look s f = lookAhead (isInfixOf s) (isInfixOf f) 

-- | Get fields if the first substring matches otherwise return empty
optLine :: String -> String -> LogState [Field]
optLine f g = do
  b <- lookAhead (isInfixOf f) (isInfixOf g)
  if b then matchLine f
   else return []

-- | Get field if the first substring matches otherwise return a default value
optField :: Read a => String -> String -> Int -> a -> LogState a
optField f g i x = do
  b <- lookAhead (isInfixOf f) (isInfixOf g)
  if b then do 
      h <- matchLine f
      return $ field i h
     else return x

-- | Get string field if the first substring matches otherwise return 
-- a default value
optString :: String -> String -> Int -> Int -> String -> LogState Field
optString f g i j x = do
  b <- lookAhead (isInfixOf f) (isInfixOf g)
  if b then do 
      h <- matchLine f
      return $ string i j h
     else return x

-- | Discard the matching lines 
goto :: [String] -> LogState [Field]
goto ss = do 
  mapM_ matchLine ss
  return []

-- | Test the first predicate until the second predicate  
lookAhead :: (String -> Bool) -> (String -> Bool) -> LogState Bool
lookAhead p g = do
   xs <- get
   let ys = dropWhile (\x -> (not . p) x && (not . g) x) xs
   if null ys || g (head ys) then return False else return True
