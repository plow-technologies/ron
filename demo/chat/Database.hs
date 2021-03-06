module Database
  ( databaseToUIUpdater
  , loadAllMessages
  , messagePoster
  , newMessage
  ) where

import           Control.Concurrent.STM (TChan, atomically, readTChan,
                                         writeTChan)
import           Control.Monad (forever, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (sortOn)
import           Data.Maybe (catMaybes)
import           Data.Traversable (for)
import           RON.Data.ORSet.Experimental (ORSet)
import qualified RON.Data.ORSet.Experimental as ORSet
import           RON.Error (MonadE)
import           RON.Event (ReplicaClock)
import           RON.Store (MonadStore, newObject, openNamedObject, readObject)
import           RON.Store.FS (runStore)
import qualified RON.Store.FS as Store
import           RON.Types (Atom (AString), ObjectRef (ObjectRef))

import           Types (MessageContent (MessageContent), MessageView, postTime)
import qualified Types

loadAllMessages :: Store.Handle -> IO [MessageView]
loadAllMessages db =
  runStore db $ do
    gMessages   <- openMessages
    mMessageSet <- readObject gMessages
    case mMessageSet of
      Nothing -> do
        liftIO $ putStrLn "!!! messages collection doesn't exist !!!"
        pure []
      Just messageSet -> do
        messageRefs <- ORSet.toList messageSet
        sortOn postTime . catMaybes <$> for messageRefs readObject

openMessages ::
  (MonadE m, MonadStore m, ReplicaClock m) =>
  m (ObjectRef (ORSet (ObjectRef MessageView)))
openMessages = openNamedObject "messages"

newMessage ::
  (MonadE m, MonadStore m, ReplicaClock m) =>
  MessageContent -> m (ObjectRef MessageView)
newMessage MessageContent{username, text} = do
  gMessages <- openMessages
  msgRef <- newObject @MessageView
  ORSet.add_ msgRef ("username", [AString username])
  ORSet.add_ msgRef ("text",     [AString text    ])
  ORSet.add_ gMessages msgRef
  pure msgRef

messagePoster :: TChan MessageContent -> Store.Handle -> IO ()
messagePoster onMessagePosted db =
  forever $ do
    message <- atomically $ readTChan onMessagePosted
    runStore db $ newMessage message

databaseToUIUpdater :: Store.Handle -> TChan [MessageView] -> IO ()
databaseToUIUpdater db onMessageListUpdated = do
  ObjectRef messageSetId <- runStore db openMessages
  Store.subcribeToObject db messageSetId
  onObjectChanged <- Store.fetchUpdates db
  forever $ do
    objectId <- atomically $ readTChan onObjectChanged
    when (objectId == messageSetId) $ do
      messages <- loadAllMessages db
      atomically $ writeTChan onMessageListUpdated messages
    -- ignore other changes
