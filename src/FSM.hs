module FSM (
    State (State), 
    StateSet(StateSet), 
    AlphabetSymbol (AlphabetSymbol),
    Alphabet (Alphabet), 
    TransitionFunction (TransitionFunction), 
    FSM (FSM),
    FSM (states),
    FSM (alphabet),
    FSM (transitionFunction),
    FSM (initialState),
    FSM (acceptingStates),
    FSM (currentState), 
    buildFSM,
    doTransition, 
    isCurrentStateAcceptState
    ) where

    import Data.Set
    import Data.Foldable (find)
    import Data.List (lookup, map, filter)
    import Data.Maybe (fromJust)


    newtype State = State (Int) deriving (Eq, Ord, Show) --only positive integers

    newtype StateSet = StateSet (Set State) deriving Show
    stateInSet :: State -> StateSet -> Bool
    stateInSet s (StateSet stateSet) = Data.Set.member s stateSet

    newtype AlphabetSymbol = AlphabetSymbol String deriving (Eq, Ord, Show)

    newtype Alphabet = Alphabet (Set AlphabetSymbol)
    symbolInAlphabet :: AlphabetSymbol -> Alphabet -> Bool
    symbolInAlphabet symb (Alphabet alphabet) = Data.Set.member symb alphabet

    newtype TransitionFunction = TransitionFunction { runTransitionFunction :: (State, AlphabetSymbol) -> State }

    data FSM = FSM { states :: StateSet
               , alphabet :: Alphabet
               , transitionFunction :: TransitionFunction
               , initialState :: State
               , acceptingStates :: StateSet
               , currentState :: State
               }

    data FSMError = InvalidState State | UndefinedState State | UndefinedSymbol AlphabetSymbol deriving (Show)

    buildFSM :: StateSet -> Alphabet -> [((State, AlphabetSymbol), State)] -> State -> StateSet -> Either FSMError FSM
    buildFSM _states@(StateSet statesSet) _alphabet@(Alphabet alphabet) _transitions _initialState _acceptingStates@(StateSet acceptingStates)
        | not (wrongState == Nothing) = Left $ InvalidState (fromJust wrongState) -- all state positive integers
        | not (elem _initialState statesSet) = Left $ UndefinedState (_initialState) -- initial state in states set
        | not (invalidTransitionStates == []) = Left $ UndefinedState $ (head invalidTransitionStates) -- all states in transitions in states set
        | not (invalidTransisitonSymbols == []) = Left $ UndefinedSymbol $ (head invalidTransisitonSymbols) -- all symbols in transitions in symbol set
        | not (invalidAcceptingSymbols == []) = Left $ UndefinedState $ (head invalidAcceptingSymbols) -- all accepting states in state sets
        | otherwise =
            Right
            FSM {
                states = _states, 
                alphabet = _alphabet, 
                transitionFunction = transition, 
                initialState = _initialState,
                acceptingStates = _acceptingStates,
                currentState = _initialState
            }
        where 
            statesList = Prelude.map (\ (State s) -> s) (Data.Set.toList statesSet)
            wrongState = case find (< 1) statesList of
                Just s -> Just (State s)
                Nothing -> Nothing
            statesInTransition = (Prelude.map (fst. fst) _transitions) ++ (Prelude.map (fst. fst) _transitions)
            symbolsInTransition = Prelude.map (snd . fst) _transitions
            invalidTransitionStates = Data.List.filter (\s -> notMember s statesSet) statesInTransition
            invalidTransisitonSymbols = Data.List.filter (\s -> notMember s alphabet) symbolsInTransition
            invalidAcceptingSymbols = Data.List.filter (\s -> notMember s statesSet) (Data.Set.toList acceptingStates)

            devilState = State (-1)
            transition = TransitionFunction (\ a -> 
                                case lookup a _transitions of
                                    Just newState -> newState
                                    Nothing -> devilState
                                )

    isCurrentStateAcceptState :: FSM -> Bool
    isCurrentStateAcceptState fsm = let cState = currentState fsm 
                                        aStates = acceptingStates fsm
                                    in stateInSet cState aStates

    doTransition :: FSM -> AlphabetSymbol -> Either FSMError FSM
    doTransition fsm s
        | symbolInAlphabet s (alphabet fsm) = Right $ fsm { currentState = newState }
        | otherwise = Left $ UndefinedSymbol s
        where newState = runTransitionFunction (transitionFunction fsm) (currentState fsm, s)