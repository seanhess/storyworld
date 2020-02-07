{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module NPC where


import           Control.Applicative     ((<|>))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson              (FromJSON, ToJSON, defaultOptions,
                                          fieldLabelModifier, genericParseJSON,
                                          genericToJSON)
import           Data.Char               (isAlphaNum)
import           Data.Maybe              (catMaybes, fromMaybe)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Data.Void               (Void)
import qualified Data.Yaml               as Yaml
import           GHC.Generics            (Generic)
import qualified System.Environment      as System
import           Text.Megaparsec         (Parsec, parseMaybe)
import qualified Text.Megaparsec         as Parse
import           Text.Megaparsec.Char    (string)

import           Data.Function           ((&))
import           Data.Monoid             ((<>))
import           Lens.Micro.Platform     (makeLenses, (.~), (^.))


import           Brick
import           Brick.Focus             (focusGetCurrent, focusRingCursor)
import           Brick.Forms             (Form, allFieldsValid, checkboxField,
                                          editPasswordField, editShowableField,
                                          editTextField, focusedFormInputAttr,
                                          formFocus, formState, handleFormEvent,
                                          invalidFields, invalidFormInputAttr,
                                          newForm, radioField, renderForm,
                                          setFieldValid, (@@=))
import qualified Brick.Widgets.Border    as B
import qualified Brick.Widgets.Center    as C
import qualified Brick.Widgets.Edit      as E
import qualified Graphics.Vty            as V




--- NPC Generation ----------------------------------------

data Command
  = Name
  | Drive
  | Look
  | Background
  | Quit
  deriving (Show, Eq)


type Parser = Parsec Void Text


parseCommand :: Parser Command
parseCommand =
      string "name" *> pure Name
  <|> string "drive" *> pure Drive
  <|> string "look" *> pure Look
  <|> string "background" *> pure Background
  <|> (string "quit" <|> string "q") *> pure Quit


data NPC = NPC
  { _name       :: Text
  , _drive      :: Text
  , _look       :: Text
  , _background :: Text
  } deriving (Show, Eq, Generic)
instance ToJSON NPC where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }
instance FromJSON NPC where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


makeLenses ''NPC


emptyNPC :: NPC
emptyNPC = NPC "" "" "" ""


npc :: MonadIO m => NPC -> m ()
npc n = do
  renderNPC n
  promptNPC n

promptNPC :: MonadIO m => NPC -> m ()
promptNPC n = do
  t <- promptNext

  case parseMaybe parseCommand t of
    Nothing -> do
      liftIO $ putStrLn "Invalid Command"
      promptNPC n
    Just Quit -> do
      liftIO $ renderNPC n
      liftIO $ putStrLn "SAVING..."
    Just c -> do
      n' <- runCommand c n
      npc n'



runCommand :: MonadIO m => Command -> NPC -> m NPC
runCommand Name n       = pure $ n & name .~ "Harold"
runCommand Drive n      = pure $ n & drive .~ "Drive"
runCommand Look n       = pure $ n & look .~ "Look"
runCommand Background n = pure $ n & background .~ "Background"
runCommand Quit n       = pure n



renderNPC :: MonadIO m => NPC -> m ()
renderNPC n = liftIO $ do
  putStrLn ""
  mapM_ putStrLn $ catMaybes
     [ line "Name" (n ^. name)
     , line "Drive" (n ^. drive)
     , line "Background" (n ^. background)
     , line "Look" (n ^. look)
     ]

  where

    line :: String -> Text -> Maybe String
    line _ "" = Nothing
    line l v  = Just $ l ++ ": " ++ (cs v)

-- can I make these separate programs? and pipe them into each other?
-- I just need to save the output as something, no?

-- you can run it in some kind of monad. Enter it. Then run the other commands. Does it work like that?
-- yeah it won't really work like that. My program exits.
-- but you could use it like an editor. Save it to disk.
-- naw this is dumb. I'm just avoiding work. I want my awesome interface.


promptNext :: MonadIO m => m Text
promptNext = liftIO $ do
  putStrLn "quit | background | look | drive | name"
  Text.getLine




-------------------------------------

data Name = NameField
          | LookField
          | DriveField
          | BackgroundField
          deriving (Eq, Ord, Show)


-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: NPC -> Form NPC e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm $
          [ label "Name" @@= editTextField name NameField (Just 1)
          , label "Look" @@= editTextField look LookField (Just 1)
          , label "Drive" @@= editTextField drive DriveField (Just 1)
          , label "Background" @@= editTextField background BackgroundField (Just 3)
          ]


theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]


draw :: Form NPC e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 1000 $ renderForm f
        help = padTop (Pad 2) $ B.borderWithLabel (str "Help?") body
        body = str $ "- Tab/Mouse to move\n" <>
                     "- Enter/Esc quit"


app :: App (Form NPC e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter [])
                    | focusGetCurrent (formFocus s) /= Just NameField -> halt s
                    -- halt s

                _ -> do
                    s' <- handleFormEvent ev s

                    -- Example of external validation:
                    -- Require age field to contain a value that is at least 18.
                    -- continue $ setFieldValid ((formState s')^.age >= 18) AgeField s'
                    continue s'

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }



filepath :: NPC -> FilePath
filepath n = cs $ "content/npcs/" <> (Text.map (\c -> if isAlphaNum c then c else '-' ) $ Text.toLower $ n ^. name) <> ".yml"



main :: IO ()
main = do
  args <- System.getArgs
  case args of
    [] -> editor Nothing emptyNPC
    [path] -> editorFile path
    _ -> putStrLn "Invalid: Either specify file or nothing"


editorFile :: FilePath -> IO ()
editorFile path = do
  n <- Yaml.decodeFileThrow path
  editor (Just path) n


editor :: Maybe FilePath -> NPC -> IO ()
editor path ninit = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        f = mkForm ninit

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app f

    putStrLn "END"
    let n = formState f'
        savepath = fromMaybe (filepath n) path

    print savepath
    putStrLn $ cs $ Yaml.encode n

    writeFile savepath (cs $ Yaml.encode n)

    -- if allFieldsValid f'
    --    then putStrLn "The final form inputs were valid."
    --    else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')
