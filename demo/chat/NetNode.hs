module NetNode (startWorkers) where

import           Control.Concurrent (threadDelay)
import           Control.Monad ((>=>))
import           Data.Foldable (for_)
import qualified Network.WebSockets as WS
import           RON.Prelude (ByteStringL)
import qualified RON.Store.FS as Store
import           RON.Text.Parse (parseOpenFrame)
import           RON.Types (OpenFrame, UUID)
import           System.Exit (ExitCode (ExitFailure))
import           System.IO (hPutStrLn, stderr)
import           System.Posix.Process (exitImmediately)

import           Database (chatroomUuid)
import           Fork (fork)

startWorkers ::
  Store.Handle ->
  -- | Server port to listen
  Maybe Int ->
  -- | Other peer ports to connect (only localhost)
  [Int] ->
  IO ()
startWorkers db listen peers =
  do
    for_ listen $ \port -> fork $ WS.runServer "::" port     server
    for_ peers  $ \port -> fork $ WS.runClient "::" port "/" client
  where
    server = WS.acceptRequest >=> dialog db
    client = dialog db

dialog :: Store.Handle -> WS.Connection -> IO ()
dialog db conn = do
  -- send object update requests
  -- fork $
  do
    ops <- fmap fold $ Store.runStore db $ Store.loadObjectLog chatroomUuid mempty
    let netMessage = ObjectOps chatroomUuid ops
    if null ops then do
        putLog "No ops for chatroom"
      else do
        putLog $ "Log for chatroom " <> show netMessage
    WS.sendBinaryData conn $ Aeson.encode netMessage
   
    objectSubscriptions <- Store.readObjectSubscriptions db
    for_ objectSubscriptions $ \object ->
      WS.sendBinaryData conn =<< encodeNetMessage RequestChanges{object}
    threadDelay 1_000_000

  -- receive
  WS.withPingThread conn 30 (pure ()) $ do
    frameData <- WS.receiveData conn
    case parseOpenFrame frameData of
      Left e      -> error $ "NetNode.dialog: parseOpenFrame: " <> e
      Right frame -> handleIncomingFrame frame
    pure ()

handleIncomingFrame :: OpenFrame -> IO ()
handleIncomingFrame _ = error "undefined handleIncomingFrame"

newtype NetMessage = RequestChanges{object :: UUID}

encodeNetMessage :: NetMessage -> IO ByteStringL
encodeNetMessage _ = do
  hPutStrLn stderr "undefined encodeNetMessage"
  exitImmediately $ ExitFailure 56
  pure undefined
