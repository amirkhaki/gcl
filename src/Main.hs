module Main (main) where
import Parser
import Text.Megaparsec (parseTest)
import System.Environment (getArgs)





main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "❌ Please provide a file path as an argument."

    (filePath : _) -> do
      input <- readFile filePath
      parseTest pProgram input
