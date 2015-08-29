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
      let state_matches = subStrs file_contents (fst pair)
          abbr_matches = subStrs file_contents (snd pair) 
      in 
      show (length state_matches + length abbr_matches)
    states_and_abbrs = zip States.states States.abbrs

--subStrs :: String -> String -> [(Text, Text)]
subStrs str sub = 
  breakOnAll (pack sub) (pack str)
