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
--["word", "word", "word"]
  putStrLn "How many letters are there?\n eg. \"4 6\""
  lettNum <- words <$> getLine
--["3", "4", "7"]
  putStrLn "What letters do you have? Type them one by one, lowercase."
  letters <- getLine
--["a", "s", "d"]
  let matched = catMaybes $ matchWords merged letters []
--[("word", "r e m"),("word", "r e m")]
  print matched
  print $ filterMatches matched lettNum
  --print $ map reverse $ map head $ catMaybes matched

type WordNRem = Maybe ([Char], [Char])
--("word", "s a e w")

bookNames :: IO [FilePath]
bookNames = listDirectory "data"

books :: IO [FilePath]
books = map ("data/" ++) <$> bookNames
--["data/book", "data/book"]

cleanChar :: Char -> Char
cleanChar ch
  | isLetter ch = ch
  | otherwise = ' '

matchWord :: String -> String -> String -> WordNRem
-- ch is char in a word, lett is letter list
matchWord (ch:chs) lett accum
  | null chs && ch `elem` lett = Just (ch:accum, (delete ch lett))
  | ch `elem` lett  = matchWord chs (delete ch lett) (ch:accum)
  | ch `notElem` lett = Nothing

matchWords :: [String] -> String -> String -> [WordNRem]
matchWords lst lett accum = map (\w -> matchWord w lett accum) lst

filterMatches :: [([a], b)] -> [String] -> [[a]]
filterMatches lst lett = filter f (revHeads lst)
  where f = (\x -> length x == (read (head lett) :: Int))
        revHeads = fmap (reverse . fst)

mainLoop :: [WordNRem] -> [String] -> [String] -> [String] -> Maybe String
mainLoop matched lst lett accum
  | null matched = Nothing
--let matched = catMaybes $ matchWords merged letters []


















--mainLoop lettNum matched l merged
--  | null lettNum = revHeads matched
--  | otherwise = map (matchWords merged)
