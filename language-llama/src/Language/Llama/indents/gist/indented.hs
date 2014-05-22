module Main where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import "mtl" Control.Monad.State
import Control.Applicative hiding (many, (<|>))

--spaces = many (oneOf " \t")

type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndent source_name $ runParserT aParser () source_name input

input_text :: String
input_text = unlines [
    "listName:",
    "  item1",
    "  item2",
    "  item3"
  ]

input_thing :: String
input_thing = unlines [
    "baz:",
    "  item1:quz",
    "  item2:bux",
    "  item3:biz"
  ]

main :: IO ()
main = do
  putStrLn $ "Input:\n" ++ input_text
  case iParse aNamedList "indented_example" input_text of
    Left  err    -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result
  putStrLn $ "Input:\n" ++ input_thing
  case iParse (pKeyVal <* eof) "indented_example" input_thing of
    Left  err    -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result

type Name = String
type Item = String

data NamedList = NamedList Name [Item]
  deriving (Show)

data Thing = Item Item
           | KeyVal [(Name, Thing)]
           deriving (Show)

--pKeyValBlock :: IParser Thing
--pKeyValBlock = withBlock KeyVal pStartBlock pKeyVal <* spaces

pKeyValPair :: IParser (Name, Thing)
pKeyValPair = (,) <$> many1 alphaNum <* char ':' <*> pThing

pKeyVal :: IParser Thing
pKeyVal = KeyVal <$> block pKeyValPair

pThing :: IParser Thing
pThing = Item <$> many1 alphaNum <* spaces
         <|> pKeyVal

aNamedList :: IParser NamedList
aNamedList = withBlock NamedList aName anItem <* spaces

aName :: IParser Name
aName = many1 alphaNum <* char ':' <* spaces

anItem :: IParser Item
anItem = many1 alphaNum <* spaces
