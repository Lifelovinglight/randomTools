module Main where

import Network.Pcap
import Data.ByteString
import Data.Char

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
main = openLive "any" 1000 True 10000
       >>= handlePackets
       >>= print

handlePackets :: PcapHandle -> IO Int
handlePackets handle = loopBS handle (-1) showPacket

showPacket :: PktHdr -> ByteString -> IO ()
showPacket header bstr = Prelude.putStrLn $ (Prelude.filter isPrint $ Prelude.filter isAscii $ fmap chr $ fmap fromIntegral $ unpack bstr)
