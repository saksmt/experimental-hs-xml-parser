module Xml
    ( tagP
    , xmlP
    , Tag(..)
    , Attribute(..)
    ) where

import Text.Parsec
import Text.Parsec.String
import Data.Char(isSpace)
import Control.Monad
import Data.Maybe

data TagContent = None | Text String | Children [Tag] deriving Show
data Attribute = Attribute { attrName :: String, attrValue :: String } deriving Show
data Tag = Tag { tagName :: String, attributes :: [Attribute], tagValue :: TagContent } deriving Show

spacingP = skipMany $ space <|> endOfLine

xmlNameP = do
    firstChar <- letter
    rest <- optionMaybe $ many $ letter <|> digit <|> (char '-')
    return $ firstChar:(join $ maybeToList rest)

nameUnderNamespaceP = do
    colon <- char ':'
    suffix <- xmlNameP
    return $ colon : suffix

namespacedNameP = do
    nameOrNamespace <- xmlNameP
    nameUnderNamespaceOrNothing <- optionMaybe nameUnderNamespaceP
    return $ nameOrNamespace ++ (maybe "" id nameUnderNamespaceOrNothing)

nameP = namespacedNameP <?> "valid XML name"

attributeP :: Parser Attribute
attributeP = do
    spacingP
    name <- nameP
    char '='
    char '"'
    value <- many $ noneOf ['"']
    char '"'
    spacingP
    return $ Attribute name value

shortTagEndP :: Parser TagContent
shortTagEndP = do
    spacingP
    char '/'
    char '>'
    return None

fusedWithComments p = do
    skipMany $ try commentP
    value <- p
    skipMany $ try commentP
    return value

tagValueP :: Parser TagContent
tagValueP = do
    try $ many commentP
    value <- many1 $ fusedWithComments $ noneOf "<"
    guard (any (not . isSpace) value)
    return $ Text value

tagChildrenP = fmap (Children) $ many $ try tagP

cdataP = do
    string "<![CDATA["
    value <- anyChar `manyTill` string "]]>"
    return $ Text value

tagContents = try cdataP <|> try tagValueP <|> tagChildrenP

commentP = do
    string "<!--"
    anyChar `manyTill` string "-->"
    return '\0'

commentsOrSpacesP = skipMany $ space <|> endOfLine <|> try commentP

longTagEndP :: String -> Parser TagContent
longTagEndP tagName = do
    char '>'
    commentsOrSpacesP
    value <- tagContents
    commentsOrSpacesP
    char '<'
    char '/'
    string tagName
    char '>'
    return value

tagEndP tagName = try shortTagEndP <|> (longTagEndP tagName)

tagP :: Parser Tag
tagP = do
    commentsOrSpacesP
    char '<'
    tagName <- nameP
    spaces
    attributes <- many attributeP
    spaces
    value <- tagEndP tagName
    commentsOrSpacesP
    return $ Tag tagName attributes value


xmlHeaderP :: Parser ()
xmlHeaderP = string "<?xml" >> (anyChar `manyTill` string "?>") >> return () <?> "XML Header"

xmlP :: Parser Tag
xmlP = xmlHeaderP >> tagP

