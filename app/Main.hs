{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import BasePrelude
import System.Directory
import Data.Witherable (hashNub)
import Data.Array

main :: IO ()
main = do
  files <- mapM readFile =<< books
  let merged = hashNub $ words $ concatMap (map cleanChar) files
  putStrLn "How many letters are there?\n eg. \"4 6\""
  lettNum <- words <$> getLine
  putStrLn "What letters do you have? Type them one by one, lowercase."
  letters <- getLine
  let matched = catMaybes $ matchWords merged letters []
  let trulyMatched = filter (\x -> length x == (read (head lettNum) :: Int)) (revHeads matched)
  print trulyMatched
  --print $ map reverse $ map head $ catMaybes matched

bookNames :: IO [FilePath]
bookNames = listDirectory "data"

books :: IO [FilePath]
books = map ("data/" ++) <$> bookNames

cleanChar :: Char -> Char
cleanChar ch
  | isLetter ch = ch
  | otherwise = ' '

matchWord :: String -> String -> String -> Maybe [String]
-- ch is char in a word, lett is letter list
matchWord (ch:chs) lett accum
  | null chs && ch `elem` lett = Just [ch:accum, (delete ch lett)]
  | ch `elem` lett  = matchWord chs (delete ch lett) (ch:accum)
  | ch `notElem` lett = Nothing

matchWords :: [String] -> String -> String -> [Maybe [String]]
matchWords lst lett accum = map (\w -> matchWord w lett accum) lst

revHeads :: [[String]] -> [String]
revHeads = fmap (reverse . head)

secElem :: [a] -> a
secElem (x:y:xs) = y

mainLoop lettNum matched l
  | null lettNum = revHeads matched
  | otherwise = map (matchWords merged)
