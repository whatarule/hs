
import Test.DocTest

main :: IO ()
main = doctest [
    "-isrc"

  , "src/Parser.hs"
  , "src/Parsec.hs"
  ]


