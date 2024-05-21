module FSMParser where
    
    --import qualified FSM (State) as FSM

    import qualified FSM --(State (State), StateSet (StateSet))

    import Text.Parsec
    import Text.Parsec.Token (lexeme, makeTokenParser, whiteSpace)
    import Text.Parsec.String (Parser)
    import Data.Set 

    stateParser :: Parser FSM.State
    stateParser = do
        n <- many1 digit
        return $ FSM.State $ (read n :: Int)

    initialStateSetParser :: Parser FSM.State
    initialStateSetParser = do
        _ <- string "INITIAL"
        spaces
        _ <- char '='
        spaces
        _ <- char '{'
        state <- stateParser
        _ <- char '}'
        return $ state

    acceptingStateSetParser :: Parser FSM.StateSet
    acceptingStateSetParser = do
        _ <- string "ACCEPTING"
        spaces
        _ <- char '='
        spaces
        _ <- char '{'
        states <- stateParser `sepBy` char ','
        _ <- char '}'
        return $ FSM.StateSet (fromList states) -- don't reports duplicates for simplicity 

    stateSetParser :: Parser FSM.StateSet
    stateSetParser = do
        _ <- string "STATES"
        spaces
        _ <- char '='
        spaces
        _ <- char '{'
        states <- stateParser `sepBy` char ','
        _ <- char '}'
        return $ FSM.StateSet (fromList states) -- don't reports duplicates for simplicity 

    alphabetSymbolParser :: Parser FSM.AlphabetSymbol
    alphabetSymbolParser = do
        s <- many1 letter
        return $ FSM.AlphabetSymbol (read s :: String)

    alphabetParser :: Parser FSM.Alphabet
    alphabetParser = do
        _ <- string "ALPHABET"
        spaces
        _ <- char '='
        spaces
        _ <- char '{'
        states <- alphabetSymbolParser `sepBy` (char ',' >> spaces)
        _ <- char '}'
        return $ FSM.Alphabet (fromList states) -- don't reports duplicates for simplicity 


    transitionParser :: Parser ((FSM.State, FSM.AlphabetSymbol), FSM.State)
    transitionParser = do

        _ <- char '('
        state <- stateParser
        _ <- char ')'

        spaces

        _ <- string "PLUS"

        spaces

        symbol <- alphabetSymbolParser

        spaces

        _ <- string "->"
        
        spaces
        _ <- char '('
        resultState <- stateParser
        _ <- char ')'
        return $ ((state, symbol), resultState)

    transitionsParser :: Parser [((FSM.State, FSM.AlphabetSymbol), FSM.State)]
    transitionsParser = do
        _ <- string "TRANSITIONS"
        spaces
        _ <- char '='
        spaces
        _ <- char '{'
        transitions <- transitionParser `sepBy` (char ',' >> spaces)
        _ <- char '}'
        return $ transitions
    

    fsmConfigParserParser :: Parser (FSM.StateSet, FSM.Alphabet, [((FSM.State, FSM.AlphabetSymbol), FSM.State)], FSM.State, FSM.StateSet)
    fsmConfigParserParser = do
        states <- stateSetParser
        _ <- newline
        alphabet <- alphabetParser
        _ <- newline
        initialState <- initialStateSetParser
        _ <- newline
        acceptingStates <- acceptingStateSetParser
        _ <- newline
        transitions <- transitionsParser
        --return (states, alphabet, transitions, initialState, acceptingStates)
        return (states, alphabet, transitions, initialState, acceptingStates)