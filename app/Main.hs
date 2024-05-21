module Main (main) where

import Lib
import FSM
import FSMParser
import System.IO (hFlush, stdout)
import Text.Parsec
import Text.Printf


main :: IO ()
main = do
    putStrLn "Введите имя файла с конфигурацией FSM:"
    hFlush stdout
    configFile <- getLine
    configContent <- readConfigFile configFile
    
    case parse fsmConfigParserParser "" configContent of
        Left err -> print err
        Right (states, alphabet, transitions, initialState, acceptingStates) -> do
            let fsm = buildFSM states alphabet transitions initialState acceptingStates
            case fsm of 
                Right fsm -> do
                    putStrLn "Конфигурация FSM успешно считана!"
                    --print fsm
                    fsmLoop fsm
                Left err -> print err
                
readConfigFile :: FilePath -> IO String
readConfigFile path = readFile path

fsmLoop :: FSM.FSM -> IO ()
fsmLoop fsm = do
    putStrLn "Введите символ для выполнения перехода или 'exit' для выхода:"
    hFlush stdout
    input <- getLine
    if input == "exit"
        then putStrLn "Выход..."
        else do
            let symbol = FSM.AlphabetSymbol input
            putStrLn (show symbol)
            let newFSM = FSM.doTransition fsm symbol
            case newFSM of
                Right newFSM -> do
                    putStrLn $ "Новое состояние: " ++ show (FSM.currentState newFSM)
                    if FSM.isCurrentStateAcceptState newFSM 
                        then putStrLn "Оно конечное"
                        else putStrLn "Оно НЕ конечное"
                    fsmLoop newFSM
                Left e -> do 
                    printf "Ошибка %s" (show e)
                    fsmLoop fsm

