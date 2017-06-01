
module Parsec (
  ) where

--import Text.Parsec.Prim
--import Text.Parsec.Combinator
--import Text.Parsec.String (Parser)
import Text.Parsec (Parsec, ParseError, parse, many, sepBy, (<|>), count)
import Text.Parsec.Char (noneOf, alphaNum, anyChar, letter, digit)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as T


import Text.Parsec (Parsec, ParseError, parse, many, sepBy, (<|>))
import Text.Parsec.Char (noneOf, alphaNum)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as T
-- |
lexer = T.makeTokenParser haskellDef

commaSep = T.commaSep lexer
semiSep = T.semiSep lexer
parens = T.parens lexer
identifier = T.identifier lexer
-- |

-- |
-- >>> sequenceA . fmap (parse ((parens . commaSep $ many alphaNum) <|> many identifier) "") =<< parse (semiSep . many $ noneOf ";") "" "select;(1,name)"
-- Right [["select"],["1","name"]]
-- >>> sequenceA . fmap (parse ((parens . commaSep $ many alphaNum) <|> many identifier) "") =<< parse (semiSep . many $ noneOf ";") "" "select;(id,field);(1,name)"
-- Right [["select"],["id","field"],["1","name"]]
--


-- |
natural = T.natural lexer
stringLiteral = T.stringLiteral lexer
symbol = T.symbol lexer
-- |

-- |
-- >>> parse digit "" "1"
-- Right '1'
-- >>> parse digit "" "1,name"
-- Right '1'
-- >>> parse (show <$> digit) "" "1,name"
-- Right "'1'"
-- >>> parse natural "" "1,name"
-- Right 1
-- >>> parse (show <$> natural) "" "1,name"
-- Right "1"
-- >>> parse (show <$> natural <* symbol ",") "" "1,name"
-- Right "1"
-- >>> parse (pure show <*> natural <* symbol ",") "" "1,name"
-- Right "1"
--
-- >>> parse (symbol "name") "" "name"
-- Right "name"
-- >>> parse (many letter) "" "name"
-- Right "name"
-- >>> parse (many letter) "" "1,name"
-- Right ""
-- >>> parse ((pure show <*> natural <* symbol ",") *> (pure show <*> natural)) "" "1,2"
-- Right "2"
-- >>> parse ((show <$> natural <* symbol ",") *> (show <$> natural)) "" "1,2"
-- Right "2"
-- >>> parse ((++) <$> (show <$> natural <* symbol ",") <*> (show <$> natural)) "" "1,2"
-- Right "12"
--
-- >>> (flip (:) []) "aaa"
-- ["aaa"]
-- >>> reverse . flip (:) ["aaa"] $ "bbb"
-- ["aaa","bbb"]
-- >>> (++) ["aaa"] ["bbb"]
-- ["aaa","bbb"]
-- >>> parse (flip (:) [] . show <$> natural <* symbol ",") "" "1,2"
-- Right ["1"]
-- >>> parse (fmap show <$> count 1 natural <* symbol ",") "" "1,2"
-- Right ["1"]
-- >>> parse (fmap show <$> many natural <* symbol ",") "" "1,2"
-- Right ["1"]
--
-- >>> parse ((++) <$> (flip (:) [] . show <$> natural <* symbol ",") <*> (flip (:) [] . show <$> natural)) "" "1,2"
-- Right ["1","2"]
-- >>> parse ((++) <$> (flip (:) [] . show <$> natural <* symbol ",") <*> (flip (:) [] <$> many letter)) "" "1,name"
-- Right ["1","name"]
-- >>> parse ((++) <$> (flip (:) [] . show <$> (symbol "(" *> natural <* symbol ",")) <*> (flip (:) [] <$> (many letter <* symbol ")"))) "" "(1,name)"
-- Right ["1","name"]
-- >>> parse (many anyChar) "" "(1,name)"
-- Right "(1,name)"
-- >>> parse (symbol "(" *> sepBy (many letter) (symbol ",") <* symbol ")") "" "(name,aaa,bbb)"
-- Right ["name","aaa","bbb"]
-- >>> parse ((++) <$> (symbol "(" *> (fmap show <$> (many natural)) <* symbol ",") <*> sepBy (many letter) (symbol ",") <* symbol ")") "" "(1,name,aaa)"
-- Right ["1","name","aaa"]
--
-- >>> parse (many alphaNum) "" "a01"
-- Right "a01"
-- >>> parse (many identifier) "" "a01"
-- Right ["a01"]
-- >>> parse (many stringLiteral) "" "a01"
-- Right []
-- >>> parse (many stringLiteral) "" "aaa"
-- Right []
-- >>> parse (stringLiteral) "" "\"aaa\""
-- Right "aaa"
--
-- >>> parse (symbol "(" *> sepBy (many identifier) (symbol ",") <* symbol ")") "" "(name,aaa,bbb)"
-- Right [["name"],["aaa"],["bbb"]]
-- >>> parse (symbol "(" *> sepBy identifier (symbol ",") <* symbol ")") "" "(name,aaa,bbb)"
-- Right ["name","aaa","bbb"]
--
-- >>> parse (symbol "(" *> sepBy (many alphaNum) (symbol ",") <* symbol ")") "" "(name,aaa,bbb)"
-- Right ["name","aaa","bbb"]
-- >>> parse (symbol "(" *> sepBy (many alphaNum) (symbol ",") <* symbol ")") "" "(1,name,aaa)"
-- Right ["1","name","aaa"]
--
-- >>> parse (symbol "(" *> commaSep (many alphaNum) <* symbol ")") "" "(name,aaa,bbb)"
-- Right ["name","aaa","bbb"]
-- >>> parse (symbol "(" *> commaSep (many alphaNum) <* symbol ")") "" "(1,name,aaa)"
-- Right ["1","name","aaa"]
--
-- >>> parse (symbol "(" *> commaSep (many alphaNum) <* symbol ")") "" "(1, name, aaa)"
-- Right ["1","name","aaa"]
--
-- >>> parse (parens $ many alphaNum) "" "(1)"
-- Right "1"
-- >>> parse (parens . commaSep $ many alphaNum) "" "(1,name)"
-- Right ["1","name"]
-- >>> parse (semiSep . many $ noneOf ";") "" "select;(1,name)"
-- Right ["select","(1,name)"]
--
-- >>> sequenceA . fmap (parse ((parens . commaSep $ many alphaNum) <|> many identifier) "") =<< parse (semiSep . many $ noneOf ";") "" "select;(1,name)"
-- Right [["select"],["1","name"]]
-- >>> sequenceA . fmap (parse ((parens . commaSep $ many alphaNum) <|> many identifier) "") =<< parse (semiSep . many $ noneOf ";") "" "select;(id,field);(1,name)"
-- Right [["select"],["id","field"],["1","name"]]
--


parsecInputString :: Parsec String () [String]
-- |
-- >>> parse parsecInputString "" "(1,name,aaa)"
-- Right ["1","name","aaa"]
-- >>> parse parsecInputString "" "(A01,name,aaa)"
-- Right ["A01","name","aaa"]
parsecInputString = parens . commaSep $ many alphaNum


parseInputString :: String -> Either ParseError [String]
-- |
-- >>> parseInputString "(1,name,aaa)"
-- Right ["1","name","aaa"]
-- >>> parseInputString "(A01,name,aaa)"
-- Right ["A01","name","aaa"]
parseInputString = parse parsecInputString msg
  where msg = ""

-- >>> splitOn "," "1,name"
-- ["1","name"]
-- >>> splitOn "," "1, name"
-- ["1","name"]
-- >>> splitOn "," "id,field"
-- ["id","field"]
-- |
--


p = foldr (++) <$> symbol "a" <*> many (symbol "a")

-- |
-- >>> parse p "error" "aaabc"
-- Right "aaa"
-- >>> parse p "error" "bbbc"
-- Left "error" (line 1, column 1):
-- unexpected "b"
-- expecting "a"
--

