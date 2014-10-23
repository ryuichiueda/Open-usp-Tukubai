import System.Environment
import Data.ByteString.Lazy.Char8 as BS
import System.IO
import System.Exit

showUsage :: IO ()
showUsage = do System.IO.hPutStr stderr (
                "Usage    : map <num=<n>> <file> \n" ++
                "Thu Oct 23 08:52:44 JST 2014\n" ++
                "Open usp Tukubai (LINUX+FREEBSD+Mac), Haskell ver.\n")
               exitWith (ExitFailure 1) 

main :: IO ()
main = do args <- getArgs
          case args of
              ["-h"]     -> showUsage
              ["--help"] -> showUsage
              [num]      -> readF "-"  >>= BS.putStr
              [num,file] -> readF file >>= BS.putStr
              _          -> showUsage

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f
