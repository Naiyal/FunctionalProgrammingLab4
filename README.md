# Лабораторная 4
---

**Студент:** Наял М.фахд  
**ИСУ:** 336192  
**Группа:** P3310 
**Университет:** НИУ ИТМО  
**Факультет:** Программная инженерия и компьютерная техника  
**Курс:** 3-й курс  

---
# Отчет

## Цель
eDSL (embedded Domain Specific Language) для конечных автоматов. eDSL должен позволять в явном виде описывать невозможные/игнорируемые переходы между состояниями. eDSL должен быть запускаемым и генерирующим описание в формате dot (подробнее см. проект graphviz). С использованием разработанного eDSL реализовать модель (одна из, определяется вариантом задания):

Светофора. Перекрёсток с главной дорогой и пешеходной кнопкой.
## Выполнение

```haskell


data TrafficLight = Red | Yellow | Green | RedPedestrian
    deriving (Show, Eq)

data Event = ButtonPressed | Timeout
    deriving (Show, Eq)

nextState :: TrafficLight -> Event -> Maybe TrafficLight
nextState Red ButtonPressed = Just RedPedestrian
nextState RedPedestrian Timeout = Just Green
nextState Green Timeout = Just Yellow
nextState Yellow Timeout = Just Red
nextState _ _ = Nothing  

simulateTrafficLight :: TrafficLight -> [Event] -> [TrafficLight]
simulateTrafficLight state [] = [state]
simulateTrafficLight state (event:events) = 
    case nextState state event of
        Just newState -> state : simulateTrafficLight newState events
        Nothing -> state : simulateTrafficLight state events

generateDot :: [(TrafficLight, Event, TrafficLight)] -> String
generateDot transitions = 
    "digraph FSM {\n" ++
    unlines (map showTransition transitions) ++ "\n" ++ 
    "}"
  where
    showTransition (from, event, to) = 
        "  " ++ show from ++ " -> " ++ show to ++ " [label=\"" ++ show event ++ "\"];"

main :: IO ()
main = do
        putStrLn "Введите количество циклов работы светофора:"
        input <- getLine
        let cycles = read input :: Int
    
       
        let events = concatMap (\n -> if n == 1 then [ButtonPressed, Timeout] else [Timeout]) [1..cycles]
    
        let states = simulateTrafficLight Red events
        putStrLn "Состояния светофора:"
        mapM_ print states

        let transitions = [
                (Red, ButtonPressed, RedPedestrian),
                (RedPedestrian, Timeout, Green),
                (Green, Timeout, Yellow),
                (Yellow, Timeout, Red)]
        let dot = generateDot transitions  
        putStrLn "\nГраф в формате DOT:"
        putStrLn dot
```






## Выводы

В ходе выполнения лабораторной работы изучил устройство eDSL, изучил свободные монады и область их практического применения.
