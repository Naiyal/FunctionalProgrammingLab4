{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module FiniteStateMachine (generateDot, liftModel, extractFSM, FSMCommand(..)) where

import Control.Monad.Free
import Data.Text (Text)
import qualified Data.Text as T

data TrafficLight = Red | Yellow | Green | RedPedestrian
    deriving (Show, Eq)

data FSMCommand next where
    State :: TrafficLight -> next -> FSMCommand next
    Transition :: TrafficLight -> Text -> TrafficLight -> next -> FSMCommand next
    StartState :: TrafficLight -> next -> FSMCommand next
    EndState :: TrafficLight -> next -> FSMCommand next
    deriving (Eq, Show)

instance Functor FSMCommand where
    fmap f (State name next) = State name (f next)
    fmap f (Transition from label to next) = Transition from label to (f next)
    fmap f (StartState name next) = StartState name (f next)
    fmap f (EndState name next) = EndState name (f next)

type FSM = Free FSMCommand

state :: TrafficLight -> FSM ()
state name = liftF $ State name ()

transition :: TrafficLight -> Text -> TrafficLight -> FSM ()
transition from label to = liftF $ Transition from label to ()

startState :: TrafficLight -> FSM ()
startState name = liftF $ StartState name ()

endState :: TrafficLight -> FSM ()
endState name = liftF $ EndState name ()

liftModel :: FSM ()
liftModel = do
    state Red
    state RedPedestrian
    state Green
    state Yellow
    startState Red
    endState Red
    transition Red "ButtonPressed" RedPedestrian
    transition RedPedestrian "Timeout" Green
    transition Green "Timeout" Yellow
    transition Yellow "Timeout" Red

generateDot :: FSM a -> Text
generateDot fsm = T.unlines $ "digraph FSM {" : map toDot (extractFSM fsm) ++ ["}"]
  where
    toDot (State name _) = T.concat ["  ", T.pack (show name), " [shape=circle];"]
    toDot (Transition from label to _) = T.concat ["  ", T.pack (show from), " -> ", T.pack (show to), " [label=\"", label, "\"];"]
    toDot (StartState name _) = T.concat ["  start -> ", T.pack (show name), ";"]
    toDot (EndState name _) = T.concat ["  ", T.pack (show name), " [shape=doublecircle];"]

extractFSM :: FSM a -> [FSMCommand ()]
extractFSM (Free command) = case command of
    State name next -> State name () : extractFSM next
    Transition from label to next -> Transition from label to () : extractFSM next
    StartState name next -> StartState name () : extractFSM next
    EndState name next -> EndState name () : extractFSM next
extractFSM (Pure _) = []
