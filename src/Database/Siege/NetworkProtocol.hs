{-# LANGUAGE ExistentialQuantification, Rank2Types, ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wwarn #-} -- I anticipate rewriting this

module Database.Siege.NetworkProtocol where

import Prelude hiding (null)

import Data.Char
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Hoist
import Control.Monad.Error
import Control.Monad.State as State

import Database.Siege.Connection as C
import Database.Siege.SharedState as Sh
import Control.Monad.Trans.Store (StoreT(..))
import qualified Database.Siege.Recv as R
import Database.Siege.DBNode (Ref, Node, DBError)
import qualified Database.Siege.DBNode as N
import qualified Database.Siege.DBMap as M
import Database.Siege.ShowTree
import Database.Siege.DBOperation as DBOp
import Database.Siege.Commands
import Database.Siege.StringHelper
import Database.Siege.Flushable

import Network.Socket (Socket)

import Debug.Trace.Monad

-- The reason this isn't over the DBOperation monad as you might expect is that 
-- it needs to be able to handle errors and to send them back to the client 
-- which wouldn't otherwise be possible.
type NetworkOp r = ConnectionT (SharedStateT (Maybe r) (StoreT r (Node r) Identity))

convert :: NetworkOp r () -> Socket -> FVar (Maybe r) -> (forall a. StoreT r (Node r) IO a -> IO a) -> IO ()
convert op sock var fn = undefined {-
  let stage1 = withSocket sock . hoist stage2
      stage2 = withFVar var . hoist stage3
      stage3 = fn . hoist stage4
      stage4 = return . runIdentity in
        stage1 op
-}

recvCommand :: NetworkOp r [Maybe B.ByteString]
recvCommand = undefined {- do
  v <- C.recvI R.recvCommand
  case v of
    Nothing -> do
      C.send $ stToB "protocol error\r\n"
      C.close
    Just v' ->
      return v'
-}

performAlter :: (Maybe r -> DBOperation r (a, Maybe r)) -> NetworkOp r (Either DBError a)
performAlter op = undefined {-
  lift $ alter $ (\head -> do
    v <- runErrorT $ DBOp.convert $ op head
    case v of
      Right (a, r) -> return (r, Right a)
      Left e -> return (head, Left e))
-}

performRead :: (Maybe r -> DBOperation r a) -> NetworkOp r (Either DBError a)
performRead op = undefined {- do
  head <- lift Sh.get
  lift $ lift $ runErrorT $ DBOp.convert $ op head
-}

constructList :: Monad m => m (Either (Maybe a) b) -> m (Either [a] b)
constructList act = undefined {- do
  v <- act
  case v of
    Right v' -> return $ Right v'
    Left Nothing -> return $ Left []
    Left (Just v') -> do
      rest <- constructList act
      case rest of
        Right v' -> return $ Right v'
        Left arr -> return $ Left $ v':arr
-}

protocol :: NetworkOp r ()
protocol = undefined {- flip (>>) (return ()) $ flip runStateT Map.empty $ forever $ do
  c <- lift $ recvCommand
  c' <- case sequence c of
    Just c' -> return c'
    Nothing -> lift $ do
      send $ stToB "protocol error"
      close
  let com = (map toLower . bToSt . head) c'
  let args = tail c'
  case (com, args) of
    ("ping", []) ->
      lift $ sendReply $ StatusReply "PONG"
    ("show", []) -> do
      ref <- lift $ lift $ Sh.get
      tree <- lift $ lift $ lift $ pullTree ref
      traceM tree
      lift $ sendReply $ StatusReply "OK"
    ("discard", []) -> do
      lift $ sendReply $ ErrorReply "ERR DISCARD without MULTI"
    ("exec", []) ->
      lift $ sendReply $ ErrorReply "ERR EXEC without MULTI"
--    ("watch", keys) -> do
--      items <- lift $ performRead (\head -> mapM (\key -> do
--        val <- mapLookup head key
--        return (key, val)) keys)
--      items' <- lift $ MaybeT $ return items
--      mapM (\(key, val) -> do
--        m <- State.get
--        if not $ Map.member key m then do
--          traceM ("inserting", key, val)
--          State.put (Map.insert key val m)
--        else
--          return ()) items'
--      lift $ sendReply $ StatusReply "OK"
    ("unwatch", []) -> do
      State.put Map.empty
      lift $ sendReply $ StatusReply "OK"
--    ("multi", []) -> do
--      lift $ lift $ sendReply $ StatusReply "OK"
--      commands <- lift $ constructList $ do
--        c <- recvCommand
--        let com = (map toLower . bToSt . head) c
--        let args = tail c
--        case (com, args) of
--          ("discard", []) -> do
--            return $ Right $ StatusReply "OK"
--          ("exec", []) -> 
--            return $ Left Nothing
--          _ -> do
--            let c' = command com args in
--              case c' of
--                Nothing -> do
--                  lift $ sendReply $ ErrorReply "unknown command"
--                  nothing
--                Just _ -> do
--                  lift $ sendReply $ StatusReply "QUEUED"
--                  return $ Left c'
--      case commands of
--        Left commands' -> do
--          watches <- State.get
--          out <- lift $ performAlter $ runStateT $ do
--            head <- State.get
--            succeed <- foldr (\(key, val) v -> do
--              v' <- v
--              if v' then do
--                ref <- lift $ mapLookup head key
--                return $ ref == val
--              else
--                return False) (return True) (Map.toList watches)
--            if succeed then do
--              items <- sequence $ map commandToState commands'
--              return $ Just items
--            else
--              return Nothing
--          out' <- lift $ MaybeT $ return out
--          State.put Map.empty
--          lift $ lift $ sendReply $ MultiReply $ out'
--        Right rep -> do
--          lift $ lift $ sendReply rep
    _ ->
      case command com args of
        Just (Left c') -> lift $ do
          r <- performRead c'
          case r of
            Right r' -> sendReply r'
            Left N.TypeError -> sendReply $ ErrorReply "type error"
            Left _ -> sendReply $ ErrorReply "unhandled error"
        Just (Right c') -> lift $ do
          r <- performAlter c'
          case r of
            Right r' -> sendReply r'
            Left N.TypeError -> sendReply $ ErrorReply "type error"
            Left _ -> sendReply $ ErrorReply "unhandled error"
        Nothing -> do
          lift $ sendReply $ ErrorReply "unknown command"
-}
