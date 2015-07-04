-- | A custom ARP sniffer using Pcap and Attoparsec.

module Main where

import Network.Pcap
import Data.ByteString (ByteString, unpack, pack)
import Data.Char
import System.IO (hFlush, stdout)
import Numeric (showHex)
import Data.List (intercalate)
import System.Environment (getArgs)
import Control.Monad (void, liftM)
import Control.Applicative ()
import qualified Data.Attoparsec.ByteString as APB
import Data.Word

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

-- | An ARP packet.
data ArpPacket = ArpPacket { oper :: ArpOperation,
                             shw :: ArpHardwareAddr,
                             sip :: ArpIPv4,
                             thw :: ArpHardwareAddr,
                             tip :: ArpIPv4 }

instance Show ArpPacket where 
  show (ArpPacket oper' shw' sip' thw' tip') =
    unwords [show oper',
             show shw',
             show sip',
             show thw',
             show tip']

-- | A byte indicating the ARP operation.
data ArpOperation = ArpOperation Word8

instance Show ArpOperation where
  show (ArpOperation 1) = "Q"
  show (ArpOperation 2) = "A"
  show (ArpOperation b) = show b
  
-- | A 6-byte Ethernet HW address.
data ArpHardwareAddr = ArpHardwareAddr ByteString

instance Show ArpHardwareAddr where
  show (ArpHardwareAddr bstr) = 
    fmap toUpper
    . intercalate ":"
    . fmap (formatHex . (`showHex` ""))
    . take 6
    $ unpack bstr

-- | An IPv4 address.
data ArpIPv4 = ArpIPv4 ByteString

instance Show ArpIPv4 where
  show (ArpIPv4 bstr) =
    intercalate "."
    . fmap show
    . take 4
    $ unpack bstr
              
-- | Entry point.
main :: IO ()
main = void (getArgs >>= help program)
  where help :: (String -> IO ()) -> [String] -> IO ()
        help _ [] = putStrLn "Usage: arpSniffer <device>"
        help fn (arg:_) = fn arg
        
-- | Open the device and set up the handler.
program :: String -> IO ()
program device = openLive device 2048 True 10000
                 >>= handlePackets
                 >>= putStrLn
                 . ("Captured packets: " ++)
                 . show

-- | Set up the handler.
handlePackets :: PcapHandle -> IO Int
handlePackets handle = loopBS handle (-1) showPacket

-- | Packet handler, called for every read packet.
showPacket :: PktHdr -> ByteString -> IO ()
showPacket _ bstr =
  either (const (return ())) print (APB.parseOnly arpPacketParser bstr)
  >> hFlush stdout 

-- | A null parser that drops the 14-byte Ethernet header
  -- and the ARP protocol, hardware type and address size fields.
arpHeaderParser :: APB.Parser ()
arpHeaderParser = APB.take 14
                  >> APB.string arpHeader
                  >> return ()
  where arpHeader = pack [0, 1, 8, 0, 6, 4, 0]

-- | A parser for an ARP packet.
arpPacketParser :: APB.Parser ArpPacket
arpPacketParser = do
  _ <- arpHeaderParser
  oper' <- arpOperationParser
  shw' <- macParser
  sip' <- ipParser
  thw' <- macParser
  tip' <- ipParser
  return $ ArpPacket oper' shw' sip' thw' tip'

-- | A parser for an ARP operation.
arpOperationParser :: APB.Parser ArpOperation
arpOperationParser = liftM ArpOperation APB.anyWord8
    
-- | Pad a string to a given length.
padRight :: Int -> String -> String
padRight len str = str ++ replicate (len - length str) ' '

-- | Pad a byte in hex format with zeroes to 2 chars width.
formatHex :: String -> String
formatHex ax@[_] = '0' : ax
formatHex ax = ax

-- | A parser for an Ethernet HW address.
macParser :: APB.Parser ArpHardwareAddr
macParser = liftM ArpHardwareAddr (APB.take 6)

-- | A parser for an IPv4 address.
ipParser :: APB.Parser ArpIPv4
ipParser = liftM ArpIPv4 (APB.take 4)
           
