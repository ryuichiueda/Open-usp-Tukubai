import System.Environment
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO
import System.Exit
import Text.Read

showUsage :: IO ()
showUsage = do System.IO.hPutStr stderr (
                "Usage    : map <num=<n>> <file> \n" ++
                "Thu Oct 23 08:52:44 JST 2014\n" ++
                "Open usp Tukubai (LINUX+FREEBSD+Mac), Haskell ver.\n")
               exitWith (ExitFailure 1) 

die str = System.IO.hPutStr stderr ( "Error[map] : " ++ str ++ "\n")
          >> exitWith (ExitFailure 1)

main :: IO ()
main = do args <- getArgs
          case args of
              ["-h"]     -> showUsage
              ["--help"] -> showUsage
              [num]      -> readF "-"  >>= main' (getNum num)
              [num,file] -> readF file >>= main' (getNum num)
              _          -> showUsage

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

type Word   = BS.ByteString
type Key    = BS.ByteString
type SubKey = BS.ByteString
type Values = [Word]
type Line   = (Key,SubKey,Values)
type Data   = [Line]

main' :: Either String Int -> BS.ByteString -> IO ()
main' (Left  str) cs = die str
main' (Right num) cs = print h_axis -- for debug
    where d = [ makeLine num ln | ln <- BS.lines cs ]
          h_axis = hAxis $ map ( \(_,s,_) -> s ) d
          hAxis []     = []
          hAxis (e:es) = e : (hAxis $ filter ( /= e ) es )

makeLine :: Int -> BS.ByteString -> Line
makeLine num ln = (k,s,v)
    where k = BS.unwords $ take num $ BS.words ln
          s = head $ drop num $ BS.words ln
          v = drop (num + 1) $ BS.words ln
    
getNum :: String -> Either String Int
getNum ('n':'u':'m':'=':str) = getNum' (readMaybe str)
getNum _                     = Left "no num option"

getNum' :: Maybe Int -> Either String Int
getNum' Nothing = Left "invalid number for num option"
getNum' (Just n) 
 | n > 0     = Right n
 | otherwise = Left "invalid number for num option"
