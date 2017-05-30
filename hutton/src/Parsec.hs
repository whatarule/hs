
module Parsec (
  ) where

--import Text.Parsec.Prim
--import Text.Parsec.Combinator


import Text.Parsec (parse, many, count, sepBy)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, letter, digit)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as T

lexer = T.makeTokenParser haskellDef

symbol = T.symbol lexer
natural = T.natural lexer

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

-- >>> splitOn "," "1,name"
-- ["1","name"]
-- >>> splitOn "," "1, name"
-- ["1","name"]
-- >>> splitOn "," "id,field"
-- ["id","field"]
-- |
--


p :: Parser String
p = foldr (++) <$> symbol "a" <*> many (symbol "a")

-- |
-- >>> parse p "error" "aaabc"
-- Right "aaa"
-- >>> parse p "error" "bbbc"
-- Left "error" (line 1, column 1):
-- unexpected "b"
-- expecting "a"
--

