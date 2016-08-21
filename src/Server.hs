{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Network.UDP
import Network.Socket

type ActiveClient = SockAddr

data Event =
    MsgEvent Message
  | TickEvent

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
handleMessage = messageOrTimer =$= manageActiveClients [] =$= xxx

-- TODO: this is temporary, need to see how to combine sources of "ticker" and "messages"
messageOrTimer :: Conduit Message IO Event
messageOrTimer = do
  maybeMsg <- await
  case maybeMsg of
    Just msg -> do
      yield $ MsgEvent msg
      messageOrTimer
    _ -> return ()

manageActiveClients :: [ActiveClient] -> Conduit Event IO (Event, [ActiveClient])
manageActiveClients activeClients = do
  maybeEvt <- await
  case maybeEvt of
    Just evt ->
      case evt of
        MsgEvent msg ->
          let activeClients = [msgSender msg]
          in do
            yield (MsgEvent msg, activeClients)
            manageActiveClients activeClients
        TickEvent -> do
          yield (TickEvent, activeClients)
          manageActiveClients activeClients
    _ -> return ()

-- TODO: this is also a place holder that will process "ticks" and "messages" and any number of "messages"
xxx :: Conduit (Event, [ActiveClient]) IO Message
xxx = do
  maybeEvtAndActiveClients <- await
  case maybeEvtAndActiveClients of
    Just (evt, activeClients) ->
      case evt of
        MsgEvent msg -> do
          yield msg
          xxx
        TickEvent -> do
          yield Message { msgData = "tick",  msgSender = head activeClients}
          liftIO $ print activeClients
          xxx
    _ -> return ()
