import System.Environment
import Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do args <- getArgs
          case args of
              [num]      -> readF "-"  >>= BS.putStr
              [num,file] -> readF file >>= BS.putStr

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f
