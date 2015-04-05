import System.IO
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as BS

showUsage :: IO ()
showUsage = do System.IO.hPutStr stderr (
                "Usage    : mojihame [-lLABEL] <template> <data> \n" ++
                "Fri Feb  6 17:18:24 JST 2015\n" ++
                "Open usp Tukubai (LINUX+FREEBSD+Mac), Haskell ver.\n")
               exitWith (ExitFailure 1) 

die str = System.IO.hPutStr stderr ( "Error[mojihame] : " ++ str ++ "\n")
          >> exitWith (ExitFailure 1)

main :: IO ()
main = do args <- getArgs
          case args of
              [tmpf,dataf]                 -> noopt tmpf dataf
              [('-':'l':label),tmpf,dataf] -> lopt (BS.pack label) tmpf dataf
              _          -> showUsage

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

noopt :: String -> String -> IO()
noopt tmpf dataf = do t <- readF tmpf
                      d <- readF dataf
                      BS.putStr $ noopt' t (BS.words d)

noopt' :: BS.ByteString -> [BS.ByteString] -> BS.ByteString
noopt' template ws = BS.pack . concat $ map (markToStr ws) (findPos $ BS.unpack template)

markToStr :: [BS.ByteString] -> (String,Int) -> String
markToStr ws (str,pos) 
 | pos == -1        = str
 | pos > length ws  = str
 | otherwise        = str ++ BS.unpack (ws !! (pos-1))

findPos :: String -> [(String,Int)]
findPos [] = []
findPos template 
 | b == []   = [(a,-1)]
 | e == True = (a ++ "%",-1) : findPos (drop 1 b)
 | n == []   = (a ++ "%",-1) : findPos (drop 1 b)
 | otherwise = (a,read n::Int) : findPos bb
    where a = takeWhile (/= '%') template
          b = dropWhile (/= '%') template
          e = escaped (reverse a)
          n = takeWhile (\x -> x >= '0' && x <= '9') (drop 1 b)
          bb = dropWhile (\x -> x >= '0' && x <= '9') (drop 1 b)

escaped :: String -> Bool
escaped []                 = False
escaped ('\\':[])          = True
escaped ('\\':'\\':revstr) = escaped revstr
escaped ('\\':_:revstr)    = True
escaped _                  = False

lopt :: BS.ByteString -> String -> String -> IO()
lopt = undefined
