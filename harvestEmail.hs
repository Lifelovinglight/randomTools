{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | A program to harvest e-mail adresses out of text files.

-- Copyright Bo Victor Natanael Fors <krakow89@gmail.com>
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
