module ShowParser (parseShow) where

import           Data.List                              (intercalate)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P

xmlHeader :: String
xmlHeader =  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

parseShow :: String -> String
parseShow str = xmlHeader++runXmlParser showParser str

showParser :: Parser String
showParser =
  listXmlParser <|>
  tupleXmlParser <|>
  try recordXmlParser <|>
  adtParser <|>
  number <|>
  quotedString

runXmlParser :: Parser a -> String -> a
runXmlParser p str = case parse p "" str of
  Left err -> error $ "parse error at " ++ show err
  Right val -> val

otag t = "<"++t++">"
ctag t = "</"++t++">"
tag t v = concat [otag t, v, ctag t]

tagAttrs :: String -> [(String,String)] -> String -> String
tagAttrs t attrs v =
  concat [ otag (unwords $ t : map (\(k,v) -> concat [k, "=\"",v,"\""]) attrs)
         , v
         , ctag t
         ]

joinNL :: [String] -> String
joinNL = intercalate "\n"

listXmlParser = do
  ls <- brackets $ commaSep showParser
  return $ tag "list" $ joinNL $ map (tag "list-elt") ls

tupleXmlParser = do
  ls <- parens $ commaSep showParser
  return $ tag "tuple" $ unwords $ map (tag "tuple-elt") ls

recordXmlParser = do
  ti <- typeXmlIdentifier
  ls <- braces $ commaSep kvXmlParser
  return $ tagAttrs "record" [("name", ti)] (joinNL ls)

kvXmlParser = do
  k <- identifier
  symbol "="
  tagAttrs "elt" [("key", k)] <$> showParser

typeXmlIdentifier = do
  fst <- oneOf ['A' .. 'Z']
  rest <- many alphaNum
  whiteSpace
  return $ fst:rest

adtParser = tag "adt" <$> typeXmlIdentifier

quotedString = do
  s <- stringLiteral
  return $ "\""++s++"\""

number = show <$> integer

lexer = P.makeTokenParser emptyDef

parens          = P.parens lexer
brackets        = P.brackets lexer
braces          = P.braces lexer
commaSep        = P.commaSep lexer
whiteSpace      = P.whiteSpace lexer
symbol          = P.symbol lexer
identifier      = P.identifier lexer
integer         = P.integer lexer
stringLiteral   = P.stringLiteral lexer
