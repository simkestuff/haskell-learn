import qualified Data.Text.IO as TIO 
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let from = head args
    let to = last args
    content <- TIO.readFile from
    TIO.writeFile to content
