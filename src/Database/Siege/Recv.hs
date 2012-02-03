{-# OPTIONS_GHC -Wwarn #-}

module Database.Siege.Recv where

import Data.Maybe
import Data.Word
import Data.Char
import qualified Data.ByteString as B
import qualified Data.Conduit as C

import Control.Monad
import Control.Monad.Trans.Maybe

import Database.Siege.StringHelper

{-
recvCommand :: Monad m => E.Iteratee B.ByteString m (Maybe [Maybe B.ByteString])
recvCommand = runMaybeT $ do
  line <- recvLift recvLine
  expectFirstChar line '*'
  n <- MaybeT $ return $ ((maybeRead . applyReversed (drop 2) . tail . bToSt) line :: Maybe Int)
  replicateM n $ do
    line' <- recvLift recvLine
    expectFirstChar line' '$'
    m <- MaybeT $ return $ ((maybeRead . applyReversed (drop 2) . tail . bToSt) line' :: Maybe Int)
    if m >= 0 then do
      dat <- recvLift $ recv m
      _ <- recvLift $ recv 2
      return $ Just dat
     else if m == -1 then
      return Nothing
     else
      MaybeT $ return Nothing
 where
  expectFirstChar :: Monad m => B.ByteString -> Char -> MaybeT m ()
  expectFirstChar line c =
    when (B.null line || (B.head line /= (fromIntegral $ ord c))) $ do
      MaybeT $ return Nothing

  applyReversed fn = reverse . fn . reverse

  maybeRead :: Read a => String -> Maybe a
  maybeRead = fmap fst . listToMaybe . reads

  recvLift :: (Monad m) => m (Maybe [B.ByteString]) -> (MaybeT m) B.ByteString
  recvLift = MaybeT . liftM (fmap B.concat)
-}
