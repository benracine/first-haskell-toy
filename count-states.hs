import Data.List
import Data.Text hiding (length, foldl, zip)

main :: IO ()
main = do
  file_string <- readFile "./homesteading-cities.txt"
  putStrLn $ processString file_string

processString :: String -> String
processString file_contents = 
  foldl tallyOccurences "" states_and_abbrs
  where 
    tallyOccurences memo pair =
      memo ++ "\n" ++ fst pair ++ ", " ++ snd pair ++ " " ++ countOccurences pair 
    countOccurences pair = 
      show (length (subStrs file_contents (fst pair)) + length (subStrs file_contents (snd pair)))
    states = ["Alabama", "Alaska", "Arizona", "Arkansas",
      "California", "Colorado",    "Connecticut", "Delaware", "Florida", "Georgia",
      "Hawaii", "Idaho",    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
      "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
      "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",   "New Hampshire",
      "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
      "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
      "South Dakota",   "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
      "Washington", "West Virginia", "Wisconsin", "Wyoming"]
    abbrs = ["AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
      "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
      "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
      "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"]
    states_and_abbrs = zip states abbrs

subStrs :: String -> String -> [(Text, Text)]
subStrs str sub = 
  breakOnAll (pack sub) (pack str)
