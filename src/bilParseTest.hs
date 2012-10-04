import BIL
import Text.Parsec.ByteString
import Text.Parsec
import System.Environment

main = do
  targets <- getArgs
  progs   <- mapM (parseFromFile astProgParser) targets
  mapM_ print progs
