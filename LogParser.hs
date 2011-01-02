module LogParser where
import Data.List
import Data.DateTime
import Control.Monad.State

type LogState a = State [String] a 
type Field = String

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

lookAhead :: (String -> Bool) -> (String -> Bool) -> LogState Bool
lookAhead p g = do
   xs <- get
   let ys = dropWhile (\x -> (not . p) x && (not . g) x) xs
   if (null ys || g (head ys)) then return False else return True

whileJust :: Monad m => m (Maybe a) -> m [a]
whileJust p = do
  x <- p
  case x of
    Nothing -> return mzero
    Just x -> do
      xs <- whileJust p
      return $ return x `mplus` xs

matchField :: Read a => String -> Int -> LogState a
matchField s i = do
  ls <- line (isInfixOf s)
  return $ field i ls

matchLine :: String -> LogState [Field]
matchLine s = line (isInfixOf s) 

field :: Read a => Int -> [Field] -> a
field i xs = read $ xs !! i

string :: Int -> Int -> [Field] -> Field
string i j = unwords . take (j - i) . drop i

look :: String -> String -> LogState Bool
look s f = lookAhead (isInfixOf s) (isInfixOf f) 

optLine :: String -> String -> LogState [Field]
optLine f g = do
  b <- lookAhead (isInfixOf f) (isInfixOf g)
  if b then matchLine f
   else return []

optField :: Read a => String -> String -> Int -> a -> LogState a
optField f g i x = do
  b <- lookAhead (isInfixOf f) (isInfixOf g)
  if b then do 
      h <- matchLine f
      return $ field i h
     else return x

optString :: String -> String -> Int -> Int -> Field -> LogState Field
optString f g i j x = do
  b <- lookAhead (isInfixOf f) (isInfixOf g)
  if b then do 
      h <- matchLine f
      return $ string i j h
     else return x

goto :: [String] -> LogState [Field]
goto ss = do 
  mapM_ matchLine ss
  return []
