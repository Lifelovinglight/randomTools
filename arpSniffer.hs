-- | A custom ARP sniffer using Pcap and Attoparsec.

module Main where

import Network.Pcap
import Data.ByteString (ByteString, unpack, pack)
import Data.Char
import System.IO (hFlush, stdout)
import Numeric (showHex)
import Data.List (intercalate)
import System.Environment (getArgs)
import Control.Monad (void, ap, liftM)
import Control.Applicative ()
import qualified Data.Attoparsec.ByteString as APB
import Data.Word
import Control.Exception (catch, SomeException)
import System.Exit (exitWith, ExitCode(..))
import Control.Parallel
import System.IO.Unsafe

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
                             shw :: HardwareAddr,
                             sip :: ArpIPv4,
                             thw :: HardwareAddr,
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
data HardwareAddr = HardwareAddr ByteString

instance Show HardwareAddr where
  show (HardwareAddr bstr) = 
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

anyWord16 = do
  a <- APB.anyWord8
  b <- APB.anyWord8
  return (((fromIntegral :: Word8 -> Word16) b) + (byteSwap16 $ (fromIntegral :: Word8 -> Word16) a))
    
-- | Entry point.
main :: IO ()
main = catch (void (getArgs >>= help program)) exceptionHandler
  where help :: (String -> IO ()) -> [String] -> IO ()
        help _ [] = putStrLn "Usage: arpSniffer <device>"
        help fn (arg:_) = fn arg

-- | Main exception handler.
exceptionHandler :: SomeException -> IO ()
exceptionHandler exception =
  putStrLn (('\n':) (show exception))
  >> hFlush stdout
  >> exitWith exitCodeFailure
  where
    exitCodeFailure :: ExitCode
    exitCodeFailure = ExitFailure 1
        
-- | Open the device and set up the handler.
program :: String -> IO ()
program device = do
  handle <- openLive device 2048 True 10000
  setNonBlock handle False
  handlePackets handle
  
-- | Set up the handler.
handlePackets :: PcapHandle -> IO ()
handlePackets handle =
  packetStream handle
  >>= mapM_ printMaybe . parMap' showPacket

printMaybe :: Maybe String -> IO ()
printMaybe Nothing = return ()
printMaybe (Just a) = putStrLn a

packetStream :: PcapHandle -> IO [(PktHdr, ByteString)]
packetStream handle = lazyIO (nextBS handle)

-- | Turn an IO action into a lazy list.
lazyIO :: (IO a) -> IO [a]
lazyIO fn = do
  a <- fn
  b <- unsafeInterleaveIO $ lazyIO fn
  return (a : b)

-- | Lazy parallel map.
parMap' :: (a -> b) -> [a] -> [b]
parMap' fn (a:ax) = par an par au (an : au)
    where an = fn a
          au = parMap' fn ax

-- | Packet handler, called for every read packet.
showPacket :: (PktHdr, ByteString) -> Maybe String
showPacket (_, bstr) =
  either (const Nothing) (Just . show) (APB.parseOnly arpPacketParser bstr)

-- | An ethernet frame.
data EthernetHeader = EthernetHeader { etherSrc :: HardwareAddr,
                                       etherDst :: HardwareAddr,
                                       etherType :: Word16 }

-- | An ethernet header parser.
ethernetHeaderParser :: APB.Parser EthernetHeader
ethernetHeaderParser =
  return EthernetHeader
  `ap` macParser
  `ap` macParser
  `ap` anyWord16

-- | A null parser that drops the 14-byte Ethernet header
  -- and the ARP protocol, hardware type and address size fields.
arpHeaderParser :: APB.Parser ()
arpHeaderParser = void $ APB.string arpHeader
  where arpHeader :: ByteString
        arpHeader = pack [0, 1, 8, 0, 6, 4, 0]

-- | A parser for an ARP packet.
arpPacketParser :: APB.Parser ArpPacket
arpPacketParser =
  ethernetHeaderParser
  >> arpHeaderParser
  >> return ArpPacket
  `ap` arpOperationParser
  `ap` macParser
  `ap` ipParser
  `ap` macParser
  `ap` ipParser
  
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
macParser :: APB.Parser HardwareAddr
macParser = liftM HardwareAddr (APB.take 6)

-- | A parser for an IPv4 address.
ipParser :: APB.Parser ArpIPv4
ipParser = liftM ArpIPv4 (APB.take 4)
           
