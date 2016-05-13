{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import           BasePrelude
import           Data.Witherable  (hashNub)
import           System.Directory
import           System.FilePath
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  allWords <- getAllWords
  let matchAllWords = matchWords allWords
  putStrLn "How many letters are there?\n eg. \"4 6\""
  wordLengths <- map read . words <$> getLine
  putStrLn "What letters do you have? Type them one by one, lowercase."
  letters <- filter isLetter <$> getLine
  let matched = matchAllWords letters
  print matched
  print "-----------------------------------------"
  let filtered = filterMatches matched wordLengths
  print filtered
  print "-----------------------------------------"
  let matched2 = map matchAllWords (map snd filtered)
  mapM_ print matched2
  print "-----------------------------------------"
  let matched3 = zipWith extendTuple (concat matched2) (map fst filtered)
  print matched3
  print "-----------------------------------------"
  --let filtered2 = filterMatches matched3
  --print filtered2
--[("word", "r e m"),("word", "r e m")]
  --print $ map reverse $ map head $ catMaybes matched

getAllWords :: IO [String]
getAllWords = do
  bookFiles <- map ("data" </>) <$> listDirectory "data"
  books <- mapM readFile bookFiles
  --writeFile "data/merged" books
  let sanitize c | isLetter c = c
                 | otherwise = ' '
  let getWords = words . map sanitize
  return $ hashNub $ concatMap getWords books

bookNames :: IO [FilePath]
bookNames = listDirectory "data"

books :: IO [FilePath]
books = map ("data/" ++) <$> bookNames

matchWord :: String -> [Char] -> Maybe (String, [Char])
matchWord word letters = go word letters
  where
    go [] ls        = Just (word, ls)
    go (c:cs) ls
      | c `elem` ls = go cs (delete c ls)
      | otherwise   = Nothing

matchWords :: [String] -> [Char] -> [(String, [Char])]
matchWords ws letters = mapMaybe (`matchWord` letters) ws

filterMatches :: [(String, String)] -> [Int] -> [(String, String)]
filterMatches lst lett = filter (p . reverse . fst) lst
  where p x = length x == head lett

findMatches :: [String] -> [Char] -> [([String], String)]
findMatches allWords allLetters =
  case matchWords allWords allLetters of
  -- no way to match even a single word
    [] -> [([], allLetters)]
    matches -> do
      (match, leftovers) <- matches
      -- find all ways to match all other words with leftover letters
      (wordSet, finalLeftOvers) <- findMatches allWords leftovers
      return (match:wordSet, finalLeftOvers)

extendTuple :: (a, b) -> c -> (c, a, b)
extendTuple tuple e = (e, (fst tuple), (snd tuple))
