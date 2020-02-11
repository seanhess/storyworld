{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Content.Names where

import Data.String.Conversions (cs)
import Control.Applicative ((<|>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Text.Megaparsec        (Parsec, parseMaybe, takeWhile1P, single, noneOf, many, satisfy, eof, (<?>), some)
import qualified Text.Megaparsec        as Parse
import           Text.Megaparsec.Char   (string, eol, digitChar, latin1Char)



data Names = Names
  { firsts :: [FirstName]
  , lasts :: [LastName]
  }


data FirstName = FirstName
  { name      :: Text
  , gender    :: Gender
  , frequency :: Int
  } deriving (Show, Eq, Generic)

data Gender = Male | Female
  deriving (Show, Eq, Generic)

type LastName = Text

type Parser = Parsec Void Text

parseFirstName :: Parser FirstName
parseFirstName = do
    n <- field :: Parser Text
    comma <?> "1111111"
    g <- genderField
    comma <?> "2222222"
    c <- count
    comma <?> "3333333"
    _ <- field
    comma <?> "4444444"
    pure $ FirstName n g c
  where
    genderField = single 'M' *> pure Male <|> single 'F' *> pure Female

    count :: Parser Int
    count = read <$> many digitChar


parseLastName :: Parser LastName
parseLastName = do
    n <- field :: Parser Text
    comma
    restOfLine
    pure $ Text.toTitle n


parseLines :: Parser a -> Parser [a]
parseLines parseLine = do
    headers
    eol
    many (parseLine <* eol)


loadCSV :: MonadIO m => Parser a -> FilePath -> m [a]
loadCSV parseLine path = do
  input <- liftIO $ Text.readFile path
  case Parse.parse (parseLines parseLine) path input of
    Left e -> error $ show e
    Right fn -> pure fn


loadFirstNames :: MonadIO m => m [FirstName]
loadFirstNames = loadCSV parseFirstName "content/SSA_Names_DB.csv"

loadLastNames :: MonadIO m => m [LastName]
loadLastNames = loadCSV parseLastName "content/Common_Surnames_Census_2000.csv"

loadNames :: MonadIO m => m Names
loadNames = do
    Names <$> loadFirstNames <*> loadLastNames


-- loadLastNames :: MonadIO m => m [LastName]


comma = single ','

field :: Parser Text
field = cs <$> (many $ satisfy (\c -> c /= ','))

headers :: Parser Text
headers = restOfLine

restOfLine :: Parser Text
restOfLine =
  takeWhile1P (Just "Rest of Line") (not . (`elem` ['\r','\n']))

