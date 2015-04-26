import System.Environment
import System.Exit
import System.IO

showUsage :: IO ()
showUsage = hPutStr stderr (
                "Usage    : plus <n1> <n2> ... \n" ++
                "Sun Apr 26 11:08:56 JST 2015\n" ++
                "Open usp Tukubai (LINUX+FREEBSD+Mac), Haskell ver.\n")
             >> exitWith (ExitFailure 1)

die str = hPutStr stderr ( "Error[map] : " ++ str ++ "\n")
          >> exitWith (ExitFailure 1)

main = do args <- getArgs
          case args of
              ["-h"]     -> showUsage
              []         -> showUsage
              nums       -> main' nums

main' :: [String] -> IO ()
main' strs = print $ sum $ map (\n -> read n :: Int) strs
