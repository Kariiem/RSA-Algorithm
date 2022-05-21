import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char
import Data.STRef.Strict
import Primes

type Key = (Integer, Integer)

fromStringToInt :: C.ByteString -> Integer
fromStringToInt = C.foldl' step 0
  where
    step :: Integer -> Char -> Integer
    step acc ch = acc * 256 + fromIntegral (ord ch)

fromIntToString :: Integer -> C.ByteString
fromIntToString n =
  LC.toStrict $ B.toLazyByteString $ snd $ until (\(num, _) -> num == 0) step (n, mempty)
  where
    step (num, buildStr) =
      let (num_quot, num_rem) = num `quotRem` 256
          ch = C.singleton $ chr $ fromIntegral num_rem
       in (num_quot, B.byteString ch <> buildStr)
