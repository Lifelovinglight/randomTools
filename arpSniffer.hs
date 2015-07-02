module Main where

import Network.Pcap
import Data.ByteString
import Data.Char
import System.IO
import Numeric (showHex, showIntAtBase)
import Data.List (intersperse)

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
main = openLive "wlo1" 1000 True 10000
       >>= handlePackets
       >>= print

handlePackets :: PcapHandle -> IO Int
handlePackets handle = loopBS handle (-1) showPacket

showPacket :: PktHdr -> ByteString -> IO ()
showPacket _ bstr = (maybe (return ()) Prelude.putStrLn $ transformPacket bstr) >> hFlush stdout

transformPacket :: ByteString -> Maybe String
transformPacket bstr = handlePacket
                       . Prelude.drop 14
                       . fmap fromIntegral
                       . unpack $ bstr

handlePacket :: [Integer] -> Maybe String
handlePacket (0:1:8:0:6:4:0:oper:rest) | oper == 1 = maybe Nothing (Just . ("Request: " ++)) $ handle' rest
                                       | oper == 2 = maybe Nothing (Just . ("Reply: " ++)) $ handle' rest
                                       | otherwise = Nothing
  where handle' :: [Integer] -> Maybe String
        handle' packetHeader = Just $ showMac packetHeader 0 ++ " " ++ showIp packetHeader 6 ++ " " ++ showMac packetHeader 10 ++ " " ++ showIp packetHeader 16   
        handle' _ = Nothing

handlePacket rest = Nothing

showMac packetHeader offset = Prelude.concat $ Data.List.intersperse ":" $ fmap (`showHex` "") $ Prelude.take 6 $ Prelude.drop offset packetHeader

showIp packetHeader offset = Prelude.concat $ Data.List.intersperse "." $ fmap show $ Prelude.take 4 $ Prelude.drop offset packetHeader
    
