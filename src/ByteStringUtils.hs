module ByteStringUtils
  ( StrictByteString,
    fromIntToString,
    fromStringToInt,
    C.hPutStrLn,
    C.hGetLine,
    C.getLine,
    C.putStrLn,
    C.putStr,
    C.pack,
    C.unpack,
    C.concat,
    toByteString,
    printToHandle,
    processPlainText,
  )
where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char
import System.IO

type StrictByteString = C.ByteString

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

toByteString :: Show a => a -> C.ByteString
toByteString = C.pack . show

printToHandle :: Show a => Handle -> a -> IO ()
printToHandle hdl = C.hPutStrLn hdl . toByteString

processPlainText :: C.ByteString -> Integer -> [C.ByteString]
processPlainText str n
  | fromStringToInt str < n = [str]
  | otherwise = listOfSubStrings str
  where
    listOfSubStrings someStr
      | fromStringToInt someStr <= n = [someStr]
      | otherwise = listOfSubStrings left ++ listOfSubStrings right
      where
        (left, right) = C.splitAt (C.length someStr `div` 2) someStr