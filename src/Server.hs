{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Network.UDP
import Data.Conduit.TMChan
import Network.Socket

import qualified Data.ByteString.Char8 as C

type ActiveClient = SockAddr

data Event =
    RecvEvent Message
  | TickEvent Integer

main :: IO ()
main = do
  sock <- udpSocket
  bindToAddr sock "127.0.0.1" 7777
  sourceSocket sock 4096 $$ handleMessage =$ sinkToSocket sock

udpSocket :: IO Socket
udpSocket = socket AF_INET Datagram 17

bindToAddr :: Socket -> String -> PortNumber -> IO ()
bindToAddr sock hostStr port = do
  host <- inet_addr hostStr
  bind sock $ SockAddrInet port host

handleMessage :: Conduit Message IO Message
handleMessage = messageOrTick =$= manageActiveClients [] =$= xxx

ticks :: Source IO Event
ticks = do
  yield $ TickEvent 0
  liftIO $ threadDelay 10000
  yield $ TickEvent 1
  liftIO $ threadDelay 10000
  yield $ TickEvent 2
  liftIO $ threadDelay 10000
  yield $ TickEvent 3

-- TODO: this is temporary, need to see how to combine sources of "ticker" and "messages"
messageOrTick :: Conduit Message IO Event
messageOrTick = do
  maybeMsg <- await
  case maybeMsg of
    Just msg -> do
      yield $ RecvEvent msg
      messageOrTick
    _ -> return ()

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

-- TODO: this is also a place holder that will process "ticks" and "messages" and any number of "messages"
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
            yield Message { msgData = C.pack  $ "tick " ++ show n,  msgSender = head activeClients}
          liftIO $ print activeClients
          xxx
    _ -> return ()
