import Control.Monad (when)
import qualified Data.ByteString as B
import System.IO
import Test.BenchPress

inpath, outpath :: String
inpath = "/tmp/infile"
outpath = "/tmp/outfile"

blockSize :: Int
blockSize = 4 * 1024

copyUsingByteString :: Handle -> Handle -> IO ()
copyUsingByteString inf outf = go
    where
      go = do
        bs <- B.hGet inf blockSize
        let numRead = B.length bs
        when (numRead > 0) $
          B.hPut outf bs >> go

main :: IO ()
main = bench 100 $ do
         inf <- openBinaryFile inpath ReadMode
         outf <- openBinaryFile outpath WriteMode
         copyUsingByteString inf outf
         hClose outf
         hClose inf
