{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import           Prelude hiding (show)
import           RON.Prelude (show, sortOn)

import           Control.Lens (to, (^?))
import           Data.Generics.Labels ()
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import           Data.Traversable (for)
import           System.Environment (getArgs, getProgName)
import           Text.Pretty.Simple (pPrint)

import           RON.Data.Experimental (Rep, ReplicatedObject, view)
import           RON.Data.ORSet.Experimental (ORSetRep)
import qualified RON.Data.ORSet.Experimental as ORSet
import qualified RON.Epoch as Epoch
import           RON.Error (errorContext, liftMaybe, throwError)
import           RON.Event (decodeEvent)
import           RON.Store (listObjects, newObject, readObject)
import           RON.Store.FS (newHandle, runStore)
import           RON.Types (Atom (AString))
import           RON.Types.Experimental (CollectionName)

data Message = Message
  { postTime :: UTCTime
  , username :: Text
  , text     :: Text
  }
  deriving (Show)

instance ReplicatedObject Message where
  type Rep Message = ORSetRep

  view objectId orset =
    errorContext "view @Message" $ do
      postTime <-
        liftMaybe "decode objectId" $
        decodeEvent objectId ^? #localTime . #_TEpoch . to Epoch.decode
      usernameP <-
        liftMaybe "lookup username" $ ORSet.lookupLww "username" orset
      username <-
        case usernameP of
          AString username : _ -> pure username
          _ -> throwError "Message.username is expected to be a string"
      textP <- liftMaybe "lookup text" $ ORSet.lookupLww "text" orset
      text <-
        case textP of
          AString text : _ -> pure text
          _ -> throwError "Message.text is expected to be a string"
      pure Message{..}

messagesCollection :: CollectionName
messagesCollection = "messages"

main :: IO ()
main = do
  db <- newHandle "./data"
  progName <- getProgName
  args <- getArgs
  case args of
    [] -> do
      messagesResult <-
        runStore db $ do
          messageRefs <- listObjects @Message messagesCollection
          for messageRefs readObject
      pPrint $ sortOn postTime messagesResult
    [username, text] -> do
      messageRef <-
        runStore db $ do
          messageRef <- newObject @Message messagesCollection
          ORSet.add_
            messageRef
            [AString "username", AString $ Text.pack username]
          ORSet.add_ messageRef [AString "text", AString $ Text.pack text]
          pure messageRef
      putStrLn $ "created message: " <> show messageRef
    _ ->
      putStrLn $
      unlines
        [ "Usage:"
        , ""
        , unwords [progName]
        , "\t show all messages"
        , ""
        , unwords [progName, "NAME TEXT"]
        , "\t post a message"
        ]