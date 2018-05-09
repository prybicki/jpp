import Data.Map as Map



main :: IO ()
main = do
  m <- Map.lookup "kotek" Map.empty
  putStrLn "Kotek"
