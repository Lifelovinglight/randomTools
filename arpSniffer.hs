module Main where

import Network.Pcap
import Data.ByteString
import Data.Char
import System.IO (hFlush, stdout)
import Numeric (showHex)
import Data.List (intersperse)
import System.Environment (getArgs)

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

main :: IO ()
main = getArgs
       >>= help program
       >> return ()
  where help :: ([String] -> IO ()) -> [String] -> IO ()
        help _ [] = Prelude.putStrLn "Usage: arpSniffer <device>"
        help fn argv = fn argv

program :: [String] -> IO ()
program (device:_) = openLive device 1000 True 10000
                     >>= handlePackets
                     >>= print
program _ = return ()

handlePackets :: PcapHandle -> IO Int
handlePackets handle = loopBS handle (-1) showPacket

showPacket :: PktHdr -> ByteString -> IO ()
showPacket _ bstr = (maybe (return ()) Prelude.putStrLn
                     $ transformPacket bstr)
                    >> hFlush stdout

transformPacket :: ByteString -> Maybe String
transformPacket bstr = handlePacket
                       . Prelude.drop 14
                       . fmap fromIntegral
                       . unpack $ bstr

handlePacket :: [Integer] -> Maybe String
handlePacket (0:1:8:0:6:4:0:oper:rest) | oper == 1 =
                                           maybe Nothing (Just . ("Q " ++))
                                           $ handle' rest
                                       | oper == 2 =
                                           maybe Nothing (Just . ("A " ++))
                                           $ handle' rest
                                       | otherwise = Nothing
  where handle' :: [Integer] -> Maybe String
        handle' packetHeader | Prelude.length packetHeader == 20 =
                                 Just
                                 $ showMac packetHeader 0
                                 ++ " "
                                 ++ showIp packetHeader 6
                                 ++ showMac packetHeader 10
                                 ++ " "
                                 ++ showIp packetHeader 16
                             | otherwise = Nothing
handlePacket _ = Nothing

padRight :: Int -> String -> String
padRight len str = str ++ Prelude.replicate (len - (Prelude.length str)) ' '

formatHex :: String -> String
formatHex ax@(_:[]) = '0' : ax
formatHex ax = ax

showMac :: [Integer] -> Int -> String
showMac packetHeader offset = Prelude.concat
                              $ Data.List.intersperse ":"
                              $ fmap2 toUpper
                              $ fmap (formatHex . (`showHex` ""))
                              $ Prelude.take 6
                              $ Prelude.drop offset packetHeader
  where fmap2 :: (a -> a) -> [[a]] -> [[a]]
        fmap2 = fmap . fmap

showIp :: [Integer] -> Int -> String
showIp packetHeader offset = padRight ((3 * 4) + 2)
                             $ Prelude.concat
                             $ Data.List.intersperse "."
                             $ fmap show
                             $ Prelude.take 4
                             $ Prelude.drop offset packetHeader
    
