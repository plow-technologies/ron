import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (newTChanIO)
import           RON.Store.FS (newHandle, runStore)
import           Text.Pretty.Simple (pPrint)

import           Database (loadAllMessages, newMessage)
import qualified Database
import           Options (Command (Post, Show, UI), UIOptions (UIOptions),
                          parseCommand)
import qualified Options
import           Types (Env (Env), MessageContent (MessageContent))
import qualified Types
import           UI (runUI)

main :: IO ()
main = do
  cmd <- parseCommand
  db <- newHandle "./data"
  case cmd of
    Show -> loadAllMessages db >>= pPrint
    Post username text -> do
      messageRef <- runStore db $ newMessage MessageContent{username, text}
      putStrLn $ "created message: " <> show messageRef
    UI UIOptions{username} -> do
      newMessageChan <- newTChanIO
      let env = Env{username, newMessageChan}
      _ <- forkIO $ Database.worker db newMessageChan
      runUI db env
