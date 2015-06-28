{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | A program to harvest e-mail adresses out of text files.
  -- (c) Victor Fors <krakow89@gmail.com>

module Main where

import Text.Parsec
import Text.Parsec.String ()
import Text.Parsec.Char ()
import System.Environment (getArgs)
import System.IO ()
import Data.List (intersperse)
import Data.Functor.Identity
import Control.Exception (catch, SomeException)

main :: IO ()
main = catch (getArgs >>= help program) handleExceptions
  where help :: ([String] -> IO ()) -> [String] -> IO ()
        help _ [] = putStrLn "Usage: harvestEmail <files>"
        help fn argv = fn argv

handleExceptions :: SomeException -> IO ()
handleExceptions exception = print exception
        
program :: [String] -> IO ()
program [] = return ()
program (arg:argv) =
  readFile arg >>=
  return . runParserState (many emailParser) [] >>=
  either print (mapM_ putStrLn) >>
  program argv

runParserState :: forall s a t a1.
                  Stream s Data.Functor.Identity.Identity t =>
                  ParsecT s a Data.Functor.Identity.Identity a1
                  -> a -> s -> Either ParseError a
runParserState parser state arg = runParser (parser >> getState) state "" arg 

emailParser :: Parsec String [String] ()
emailParser = try emailParser' <|> (anyToken >> (emailParser <|> (eof >> return ())))

emailParser' :: Parsec String [String] ()
emailParser' = do
  namePart <- namePartParser
  _ <- (char '@')
  addressParts <- sepEndBy1 domainPartParser (char '.')
  domainPart <- tldParser
  modifyState ((namePart ++ "@" ++ (concat $ intersperse "." (addressParts ++ [domainPart]))) :)
  return ()

namePartParser :: Parsec String [String] String
namePartParser = many1 alphaNum

notAlphaNum :: Parsec String u Char
notAlphaNum = noneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

domainPartParser :: Parsec String [String] String
domainPartParser = do
  domainPart <- many1 alphaNum
  _ <- char '.'
  return domainPart

endOfEmailParser :: Parsec String [String] ()
endOfEmailParser = notAlphaNum >> return ()

tldParser :: Parsec String [String] String
tldParser = do
  tldPart <- try (count 3 letter) <|> (count 2 letter)
  _ <- endOfEmailParser
  return tldPart
