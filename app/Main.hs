module Main (main) where

import FiniteStateMachine
import qualified Data.Text as T

main :: IO ()
main = do
    -- Запросить количество циклов работы светофора
    putStrLn "Введите количество циклов работы светофора:"
    input <- getLine
    let cycles = read input :: Int

    -- Создать события для светофора
    let events = concatMap (\n -> if n == 1 then [ButtonPressed, Timeout] else [Timeout]) [1..cycles]

    -- Симуляция работы светофора
    let states = simulateTrafficLight Red events
    putStrLn "Состояния светофора:"
    mapM_ print states

    -- Определение переходов для генерации графа
    let transitions = [
            (Red, ButtonPressed, RedPedestrian),
            (RedPedestrian, Timeout, Green),
            (Green, Timeout, Yellow),
            (Yellow, Timeout, Red)
        ]

    -- Генерация графа в формате DOT
    let dot = generateDot transitions
    putStrLn "\nГраф в формате DOT:"
    putStrLn dot
