{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import qualified NPC


import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.String.Conversions    (cs)
import           Data.Text                  (Text)
import qualified Data.Text as Text
import qualified Data.Yaml                  as Yaml
import           GHC.Generics               (Generic)
import           System.Console.Pretty      (Style (..), style)
import           System.Random              (randomRIO)


data Names = Names
  { male   :: [Text]
  , female :: [Text]
  } deriving (Eq, Show, Generic)
instance FromJSON Names
instance ToJSON Names


main :: IO ()
main = simpleMain ui



-- Note: we should USE UP names. Ooh :)
-- not safe at all :)

randomElement :: MonadIO m => [a] -> m a
randomElement [] = error "empty list"
randomElement [a] = pure a
randomElement as = do
  i <- liftIO $ randomRIO (0, length as - 1)
  pure $ as !! i






ui :: Widget ()
ui =
    withBorderStyle unicode $
    borderWithLabel (str "Hello!") $
    (center (str "Left") <+> vBorder <+> center (str "Right"))






--- Hard moves --------------------------------------------

data CityRules = CityRules
  { hardmoves :: [HardMove]
  } deriving (Show, Eq, Generic)
instance FromJSON CityRules

data HardMove = HardMove
  { name        :: Text
  , description :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON HardMove

hardmove :: IO ()
hardmove = do
    city <- Yaml.decodeFileThrow "./content/cityofmist.yaml" :: IO CityRules
    move <- randomElement (hardmoves city)
    renderHardMove move


renderHardMove :: HardMove -> IO ()
renderHardMove move = do
    putStrLn $ style Bold . style Underline $ cs (name move)
    putStrLn ""
    renderMarkup (description move)


renderMarkup :: Text -> IO ()
renderMarkup text = do
  let ls = Text.lines text
  mapM_ (putStrLn . markup) ls

markup :: Text -> String
markup t
  | "> " `Text.isPrefixOf` t = style Italic $ cs $ Text.drop 2 t
  | otherwise = cs $ t
