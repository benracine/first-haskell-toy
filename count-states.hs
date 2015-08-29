import Data.List (foldl, length)
import Data.Text (breakOnAll, pack)
import States

main :: IO ()
main = do
  file_string <- readFile "./homesteading-cities.txt"
  putStrLn $ processString file_string

--processString :: String -> String
processString file_contents = 
  foldl tallyOccurences [] states_and_abbrs
  where 
    tallyOccurences memo pair =
      memo ++ "\n" ++ fst pair ++ ", " ++ snd pair ++ " " ++ countOccurences pair 
    countOccurences pair = 
      show (length (subStrs file_contents (fst pair)) + length (subStrs file_contents (snd pair)))
    states_and_abbrs = zip States.states States.abbrs

--subStrs :: String -> String -> [(Text, Text)]
subStrs str sub = 
  breakOnAll (pack sub) (pack str)
