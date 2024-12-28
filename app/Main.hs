module Main (main) where

import FiniteStateMachine
import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "Введите количество циклов работы светофора:"
    input <- getLine
    let cycles = read input :: Int

    let events = concatMap (\n -> if n == 1 then [ButtonPressed, Timeout] else [Timeout]) [1..cycles]

    let states = simulateTrafficLight Red events
    putStrLn "Состояния светофора:"
    mapM_ print states

    let dot = generateDot liftModel
    putStrLn "\nГраф в формате DOT:"
    putStrLn (T.unpack dot)
