{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Network.UDP
import Data.Conduit.TMChan
import Network.Socket

import qualified Data.ByteString.Char8 as C

import Game (game)

type ActiveClient = SockAddr

data Event =
    RecvEvent Message
  | TickEvent Integer

main :: IO ()
main = do
  sock <- liftIO udpSocket
  bindToAddr sock "127.0.0.1" 7777
  runResourceT $ do
    mergedEvents <- mergeSources [recvEvents sock 4096, tickEvents 0 1000] 16
    liftIO (mergedEvents $$ handleEvents =$ sinkToSocket sock)

udpSocket :: IO Socket
udpSocket = socket AF_INET Datagram 17

bindToAddr :: Socket -> String -> PortNumber -> IO ()
bindToAddr sock hostStr port = do
  host <- inet_addr hostStr
  bind sock $ SockAddrInet port host

recvEvents :: Socket -> Int -> Source (ResourceT IO) Event
recvEvents sock maxLen = mapOutput RecvEvent $ sourceSocket sock maxLen

tickEvents :: Integer -> Int -> Source (ResourceT IO) Event
tickEvents n delayMillis = do
  yield $ TickEvent n
  liftIO $ threadDelay (delayMillis * 1000)
  tickEvents (n + 1) delayMillis

handleEvents :: Conduit Event IO Message
handleEvents = manageActiveClients [] =$= xxx

-- TODO: really manage active clients, not only the client that sent the last message
manageActiveClients :: [ActiveClient] -> Conduit Event IO (Event, [ActiveClient])
manageActiveClients activeClients = do
  maybeEvt <- await
  case maybeEvt of
    Just evt ->
      case evt of
        RecvEvent msg ->
          let activeClients = [msgSender msg]
          in do
            yield (RecvEvent msg, activeClients)
            manageActiveClients activeClients
        TickEvent n -> do
          yield (TickEvent n, activeClients)
          manageActiveClients activeClients
    _ -> return ()

-- TODO: process "events" and produce any number of "messages" (temporary placeholder)
xxx :: Conduit (Event, [ActiveClient]) IO Message
xxx = do
  maybeEvtAndActiveClients <- await
  case maybeEvtAndActiveClients of
    Just (evt, activeClients) ->
      case evt of
        RecvEvent msg -> do
          yield msg
          xxx
        TickEvent n -> do
          unless (null activeClients) $
            yield Message { msgData = C.pack  $ "tick " ++ show n ++ " " ++ game ++ "\n",  msgSender = head activeClients}
          liftIO $ print activeClients
          xxx
    _ -> return ()
