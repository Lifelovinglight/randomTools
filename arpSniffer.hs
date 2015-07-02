-- | A custom ARP sniffer using Pcap.

module Main where

import Network.Pcap
import Data.ByteString (ByteString, unpack)
import Data.Char
import System.IO (hFlush, stdout)
import Numeric (showHex)
import Data.List (intercalate)
import System.Environment (getArgs)
import Control.Monad (void)

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

-- | Entry point.
main :: IO ()
main = void (getArgs >>= help program)
  where help :: ([String] -> IO ()) -> [String] -> IO ()
        help _ [] = putStrLn "Usage: arpSniffer <device>"
        help fn argv = fn argv
        
-- | Open the device and set up the handler.
program :: [String] -> IO ()
program (device:_) = openLive device 2048 True 10000
                     >>= handlePackets
                     >>= putStrLn . ("Captured packets: " ++) . show 
program _ = return ()

-- | Set up the handler.
handlePackets :: PcapHandle -> IO Int
handlePackets handle = loopBS handle (-1) showPacket

-- | Packet handler, called for every read packet.
showPacket :: PktHdr -> ByteString -> IO ()
showPacket _ bstr = maybe (return ()) putStrLn (transformPacket bstr)
                    >> hFlush stdout

-- | First layer dissector, drop ethernet header.
transformPacket :: ByteString -> Maybe String
transformPacket bstr = handlePacket
                       . drop 14
                       . fmap fromIntegral
                       . unpack $ bstr

-- | Second layer dissector, pattern match on ARP protocol.
handlePacket :: [Int] -> Maybe String
handlePacket (0:1:8:0:6:4:0:oper:rest) | oper == 1 =
                                           fmap ("Q " ++)
                                           $ handle' rest
                                       | oper == 2 =
                                           fmap ("A " ++)
                                           $ handle' rest
                                       | otherwise = Nothing
  where handle' :: [Int] -> Maybe String
        handle' packetHeader | length packetHeader == 20 =
                                 Just
                                 $ showMac packetHeader 0
                                 ++ " "
                                 ++ showIp packetHeader 6
                                 ++ showMac packetHeader 10
                                 ++ " "
                                 ++ showIp packetHeader 16
                             | otherwise = Nothing
handlePacket _ = Nothing

-- | Pad a string to a given length.
padRight :: Int -> String -> String
padRight len str = str ++ replicate (len - length str) ' '

-- | Pad a byte in hex format with zeroes to 2 chars width.
formatHex :: String -> String
formatHex ax@(_:[]) = '0' : ax
formatHex ax = ax

-- | Display a HW address from a certain offset in a packet header.
showMac :: [Int] -> Int -> String
showMac packetHeader offset = fmap toUpper
                              . intercalate ":"
                              . fmap (formatHex . (`showHex` ""))
                              . take 6
                              $ drop offset packetHeader

-- | Display an IPv4 adress from a certain offset in a packet header.
showIp :: [Int] -> Int -> String
showIp packetHeader offset = padRight ((3 * 4) + 2)
                             . intercalate "."
                             . fmap show
                             . take 4
                             $ drop offset packetHeader
    
