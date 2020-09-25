import qualified Clash.Main as Clash
import System.Directory

main :: IO ()
main = do
    putStrLn $ "Clash.defaultMain " <> unwords args
    createDirectoryIfMissing True "_build"
    setCurrentDirectory "_build"
    Clash.defaultMain args
  where
    args = ["-odir .", "-hidir .", "-i../src", "--verilog", "../src/top.hs"]
