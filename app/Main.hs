{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (readFile, putStrLn, length, getContents)
import FrequentSubstring ( mostFrequentSubstringN
                         , countSubsequencesWithLengthAtLeast
                         , countSubsequencesOfLength
                         , longestSubstringWithRRepeats
                         )
import Options.Applicative
import Data.Text.IO (readFile, putStrLn, getContents)
import Data.Text as T (Text, pack)
import qualified Data.Text as T (length)
import Data.Text.Lazy (fromStrict, toStrict)
import Util (showText)
import Text.Replace (Replace(Replace), replaceWithList, text'fromText)

data Options = Options
  (Maybe FilePath) -- inputPath
  (Maybe Int) -- length; used to find the most frequent subsequence of the specified length
  (Maybe Int) -- repeats; used to find the longest subsequence which repeats this many times
  (Maybe String) -- substitute; replace the subsequence found with this
  Bool -- isVerbose

options :: Parser Options
options = Options
      <$> optional (strOption
          ( long "filepath"
         <> short 'i'
         <> metavar "FilePath"
         <> help "path to input file containing the text sequence"
          ))
      <*> optional (option auto
          ( long "length"
         <> short 'l'
         <> help "find the most frequent subsequence of a specified length"
         <> metavar "INT"
          ))
      <*> optional (option auto
          ( long "repeats"
         <> short 'r'
         <> help "find the longest subsequence which repeats this many times"
         <> metavar "INT"
          ))
      <*> optional ( strOption
          ( long "substitute"
         <> short 's'
         <> metavar "String"
         <> help "replace the subsequence found with a specified string"
          ))
      <*> switch
          ( long "verbose"
         <> short 'v'
         <> help "whether to display counts, etc."
          )

main :: IO ()
main = greet =<< execParser opts
  where opts = info (options <**> helper)
                 ( fullDesc
                <> progDesc "find frequent subsequences in a text file"
                <> header "frequent-substring help:"
                 )

greet :: Options -> IO ()
greet (Options _ Nothing Nothing Nothing _) = do
  putStrLn "No length nor number-of-repeats was specified; nothing to do."
greet (Options inputPathMb (Just length) Nothing Nothing isVerbose) = do
  if isVerbose then do
    putStrLn $ "Finding the most frequent subsequence of length "
            <> showText length <> " in the content of "
            <> nameInputSource inputPathMb
    else return ()
  content <- getSequence inputPathMb
  if isVerbose then do
    let contentLength = T.length content
    putStrLn $ "content has length: " <> showText contentLength
    putStrLn $ "number of subsequences of exact length " <> showText length <> ": "
            <> showText (countSubsequencesOfLength contentLength length)
  else return ()
  let (substring,count) = mostFrequentSubstringN length content
  if isVerbose then do
    putStrLn "substring:"
    putStrLn substring
    putStrLn $ "length: " <> showText (T.length substring)
    putStrLn $ "repeats: " <> showText count
  else do
    putStrLn substring
greet (Options inputPathMb Nothing (Just repeats) Nothing isVerbose) = do
  if isVerbose then do
    putStrLn $ "Finding the longest subsequence that repeats "
            <> showText repeats
            <> " times, in the content of "
            <> nameInputSource inputPathMb
    else return ()
  content <- getSequence inputPathMb
  if isVerbose then do
    let contentLength = T.length content
    putStrLn $ "content has length: " <> showText contentLength
    putStrLn $ "number of subsequences with length at least " <> showText (0::Int) <> ": "
            <> showText (countSubsequencesWithLengthAtLeast contentLength 0)
  else return ()
  let (substring, count) = longestSubstringWithRRepeats repeats content
  if isVerbose then do
    putStrLn "substring:"
    putStrLn substring
    putStrLn $ "length: " <> showText (T.length substring)
    putStrLn $ "repeats: " <> showText count
  else do
    putStrLn substring
greet (Options _ Nothing Nothing (Just _) _) = do
  putStrLn "No length nor number-of-repeats was specified; nothing to do."
greet (Options _ (Just _) (Just _) Nothing _) = do
  putStrLn "Both length and number-of-repeats was specified; this functionality does not yet exit."
greet (Options inputPathMb (Just length) Nothing (Just substitute) isVerbose) = do
  if isVerbose then do
    putStrLn $ "Finding the most frequent subsequence of length "
            <> showText length <> " in the content of "
            <> nameInputSource inputPathMb
    else return ()
  content <- getSequence inputPathMb
  if isVerbose then do
    let contentLength = T.length content
    putStrLn $ "content has length: " <> showText contentLength
    putStrLn $ "number of subsequences of exact length " <> showText length <> ": "
            <> showText (countSubsequencesOfLength contentLength length)
  else return ()
  let (substring,count) = mostFrequentSubstringN length content
  let replaced = substituteText content substring (pack substitute)
  if isVerbose then do
    putStrLn "substring:"
    putStrLn substring
    putStrLn $ "length: " <> showText (T.length substring)
    putStrLn $ "repeats: " <> showText count
    putStrLn "substitute:" 
    putStrLn (pack substitute)
    putStrLn $ "content of " <> nameInputSource inputPathMb <> " after replacement:"
    putStrLn replaced
  else do
    putStrLn replaced
greet (Options inputPathMb Nothing (Just repeats) (Just substitute) isVerbose) = do
  if isVerbose then do
    putStrLn $ "Finding the longest subsequence that repeats "
            <> showText repeats
            <> " times, in the content of "
            <> nameInputSource inputPathMb
    else return ()
  content <- getSequence inputPathMb
  if isVerbose then do
    let contentLength = T.length content
    putStrLn $ "content has length: " <> showText contentLength
    putStrLn $ "number of subsequences with length at least " <> showText (0::Int) <> ": "
            <> showText (countSubsequencesWithLengthAtLeast contentLength 0)
  else return ()
  let (substring, count) = longestSubstringWithRRepeats repeats content
  let replaced = substituteText content substring (pack substitute)
  if isVerbose then do
    putStrLn "substring:"
    putStrLn substring
    putStrLn $ "length: " <> showText (T.length substring)
    putStrLn $ "repeats: " <> showText count
    putStrLn "substitute:" 
    putStrLn (pack substitute)
    putStrLn $ "content of " <> nameInputSource inputPathMb <> " after replacement:"
    putStrLn replaced
  else do
    putStrLn replaced
greet (Options _ (Just _) (Just _) (Just _) _) = do
  putStrLn "Both length and number-of-repeats was specified; this functionality does not yet exit."

nameInputSource :: Maybe String -> Text
nameInputSource Nothing = "the piped input"
nameInputSource (Just inputPath) = pack inputPath

-- | If Nothing, reads from stdin.
-- | Otherwise, read from the filepath in the Just.
getSequence :: Maybe String -> IO Text
getSequence Nothing = getContents
getSequence (Just inputPath) = readFile inputPath

substituteText :: Text -> Text -> Text -> Text
substituteText content substring substitute
  = toStrict $ replaceWithList [Replace (text'fromText substring) substitute] (fromStrict content)


