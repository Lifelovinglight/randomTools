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
import Data.Bits
import System.Endian
import Control.Arrow

-- Copyright Bo Victor Natanael Fors <krakow89@gmail.com>
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANtability or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | An ARP packet.
data ArpPacket = ArpPacket { arp :: ArpProtocolNumber,
                             ahw :: ArpHardwareType,
                             apt :: ArpProtocolType,
                             hww :: ArpHardwareWidth,
                             ipw :: ArpAddressWidth,
                             oper :: ArpOperation,
                             shw :: HardwareAddr,
                             sip :: IPv4,
                             thw :: HardwareAddr,
                             tip :: IPv4 }

instance Show ArpPacket where 
  show (ArpPacket arp' ahw' apt' hww' ipw' oper' shw' sip' thw' tip') =
    unwords [show arp',
             show ahw',
             show apt',
             show hww',
             show ipw',
             show oper',
             show shw',
             show sip',
             show thw',
             show tip']

data ArpProtocolNumber = ArpProtocolNumber Word16

instance Show ArpProtocolNumber where
  show (ArpProtocolNumber 2054) = "ARP"
  show (ArpProtocolNumber n) = show n

data ArpHardwareType = ArpHardwareType Word16

instance Show ArpHardwareType where
  show (ArpHardwareType 1) = "Ethernet"
  show (ArpHardwareType n) = show n

data ArpProtocolType = ArpProtocolType Word16

instance Show ArpProtocolType where
  show (ArpProtocolType 2048) = "IPv4"
  show (ArpProtocolType n) = show n

data ArpHardwareWidth = ArpHardwareWidth Word8

instance Show ArpHardwareWidth where
  show (ArpHardwareWidth n) = show n

data ArpAddressWidth = ArpAddressWidth Word8

instance Show ArpAddressWidth where
  show (ArpAddressWidth n) = show n
  
-- | A byte indicating the ARP operation.
data ArpOperation = ArpOperation Word16

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
data IPv4 = IPv4 ByteString

instance Show IPv4 where
  show (IPv4 bstr) =
    intercalate "."
    . fmap show
    . take 4
    $ unpack bstr

(>>+) :: (a -> (a1, b)) -> (a1 -> b -> c) -> a -> c
a >>+ b = a >>> uncurry b

-- | A parser matching any 2 octets.
anyWord16 :: APB.Parser Word16
anyWord16 =
  liftM
  (sum . zipWith (curry an) byteWeights)
  $ APB.count width APB.anyWord8
  where
    byteWeights =
      reverse
      . take width
      $ 1 : [2 ^ (8 * n) | n <- [1..] :: [Word16]]
    an = second fromIntegral >>+ (*)
    width = 2

-- | A parser matching any 4 octets.
anyWord32 :: APB.Parser Word32
anyWord32 =
  liftM
  (sum . zipWith (curry an) byteWeights)
  $ APB.count width APB.anyWord8
  where
    byteWeights =
      reverse
      . take width
      $ 1 : [2 ^ (8 * n) | n <- [1..] :: [Word32]]
    an = second fromIntegral >>+ (*)
    width = 4
    
-- | A parser matching any 8 octets.    
anyWord64 :: APB.Parser Word64
anyWord64 =
  liftM
  (sum . zipWith (curry an) byteWeights)
  $ APB.count width APB.anyWord8
  where
    byteWeights =
      reverse
      . take width
      $ 1 : [2 ^ (8 * n) | n <- [1..] :: [Word64]]
    an = second fromIntegral >>+ (*)
    width = 8
    
-- | A parser matching a specific 'Word16'.
word16 :: Word16 -> APB.Parser Word16
word16 w = APB.string bytes >> return w
  where bytes =
          pack
          . fmap (fromIntegral . shift' w)
          . reverse
          . take width
          $ [0, 8 ..]
        shift' = if getSystemEndianness == BigEndian
                 then shiftL
                 else shiftR
        width = 2
        
-- | A parser matching a specific 'Word32'.
word32 :: Word32 -> APB.Parser Word32
word32 w = APB.string bytes >> return w
  where bytes =
          pack
          . fmap (fromIntegral . shift' w)
          . reverse
          . take width
          $ [0, 8 ..]
        shift' = if getSystemEndianness == BigEndian
                 then shiftL
                 else shiftR
        width = 4
        
-- | A parser matching a specific 'Word64'.
word64 :: Word16 -> APB.Parser Word16
word64 w = APB.string bytes >> return w
  where bytes =
          pack
          . fmap (fromIntegral . shift' w)
          . reverse
          . take width
          $ [0, 8 ..]
        shift' = if getSystemEndianness == BigEndian
                 then shiftL
                 else shiftR
        width = 8
    
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
program device =
  openLive device 2048 True 10000
  >>= handlePackets
  
-- | Set up the handler.
handlePackets :: PcapHandle -> IO ()
handlePackets handle =
  packetStream handle
  >>= mapM_ printMaybe . parMap' showPacket

printMaybe :: Maybe String -> IO ()
printMaybe Nothing = return ()
printMaybe (Just a) = putStrLn a

-- | Turn a pcap handle into a stream of packets.
packetStream :: PcapHandle -> IO [(PktHdr, ByteString)]
packetStream handle = 
  setNonBlock handle False
  >> lazyIO (nextBS handle)

-- | Turn an IO action into a lazy list.
lazyIO :: IO a -> IO [a]
lazyIO fn = do
  a <- fn
  b <- unsafeInterleaveIO $ lazyIO fn
  return (a : b)

-- | Lazy parallel map.
parMap' :: (a -> b) -> [a] -> [b]
parMap' _ [] = []
parMap' fn (a:ax) = par an par au (an : au)
    where an = fn a
          au = parMap' fn ax

-- | Packet handler, called for every read packet.
showPacket :: (PktHdr, ByteString) -> Maybe String
showPacket (_, bstr) =
  either (const Nothing) (Just . show) (APB.parseOnly ethernetHeaderParser bstr)

-- | An ethernet frame.
data EthernetHeader
  = EthernetHeader
    {
      etherSrc :: HardwareAddr,
      etherDst :: HardwareAddr,
      etherPayload :: EthernetPayload
    }

instance Show EthernetHeader where
  show (EthernetHeader src' dst' payload') =
    unwords [show src',
             show dst',
             show payload']
    
data EthernetPayload = EthernetArp ArpPacket

instance Show EthernetPayload where
  show (EthernetArp p) = show p
                               
-- | An ethernet header parser.
ethernetHeaderParser :: APB.Parser EthernetHeader
ethernetHeaderParser =
  return EthernetHeader
  `ap` macParser -- Source address.
  `ap` macParser -- Destination address.
  `ap` liftM EthernetArp arpPacketParser
  
-- | A parser for an ARP packet.
arpPacketParser :: APB.Parser ArpPacket
arpPacketParser =
  return ArpPacket
  `ap` arpProtocolNumberParser
  `ap` arpHardwareTypeParser
  `ap` arpProtocolTypeParser
  `ap` arpHardwareWidthParser
  `ap` arpAddressWidthParser
  `ap` arpOperationParser -- ARP operation number.
  `ap` macParser          -- Source hardware address.
  `ap` ipParser           -- Source IPv4 address.
  `ap` macParser          -- Destination hardware address.
  `ap` ipParser          -- Destination IPv4 address.

arpProtocolNumberParser :: APB.Parser ArpProtocolNumber
arpProtocolNumberParser = liftM ArpProtocolNumber (word16 2054)

arpHardwareTypeParser :: APB.Parser ArpHardwareType
arpHardwareTypeParser = liftM ArpHardwareType anyWord16

arpProtocolTypeParser :: APB.Parser ArpProtocolType
arpProtocolTypeParser = liftM ArpProtocolType anyWord16

arpHardwareWidthParser :: APB.Parser ArpHardwareWidth
arpHardwareWidthParser = liftM ArpHardwareWidth APB.anyWord8

arpAddressWidthParser :: APB.Parser ArpAddressWidth
arpAddressWidthParser = liftM ArpAddressWidth APB.anyWord8
  
-- | A parser for an ARP operation.
arpOperationParser :: APB.Parser ArpOperation
arpOperationParser = liftM ArpOperation anyWord16
    
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
ipParser :: APB.Parser IPv4
ipParser = liftM IPv4 (APB.take 4)
           
