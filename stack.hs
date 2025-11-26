
data Command 
    = Push Value
    | Mul
    | Add 
    | Div

data Value 
    = Literal Double
    | StrVal String

data RuntimeError
    = DivisionByZero
    | StackUnderflow String  
    | UnknownVariable String
    | MultipleResults [Double]
    | EmptyResult
    deriving (Show, Eq)


program = [(Push (Literal 5)),
           (Push (Literal 4)),
           (Push (StrVal "x")),
           (Mul),
           (Push (Literal 2)),
           (Push (StrVal "x")),
           (Push (StrVal "x")),
           (Mul),
           (Mul),
           (Add),
           (Add)]


type Params = [(String, Double)]
runProgramOnStack :: [Command] -> Params -> [Double] -> Either RuntimeError [Double]
runProgramOnStack commands params startStack =
    foldl f (Right startStack) commands where 
        f :: Either RuntimeError [Double] -> Command -> Either RuntimeError [Double]
        f (Left err) _ = Left err
        f (Right currStack) command =
            case command of
            (Push (Literal v)) -> Right (v:currStack)
            (Push (StrVal var)) ->    
                case lookup var params of
                Just val -> Right (val:currStack )
                Nothing -> Left (UnknownVariable var)
            Mul -> 
                case currStack of
                    x:y:rest -> Right ((x * y):rest)
                    _ -> Left (StackUnderflow "Mul")
            Add -> 
                case currStack of
                    x:y:rest -> Right ((x + y) : rest)
                    _ -> Left (StackUnderflow "Add")
            Div -> 
                case currStack of
                    _:0:rest -> Left (DivisionByZero)
                    x:y:rest -> Right ((x / y) : rest)
                    _ -> Left (StackUnderflow "Div")

runProgram :: [Command] -> Params -> Either RuntimeError Double 
runProgram commands params = 
    case runProgramOnStack commands params [] of
        Left err -> Left err
        Right [] -> Left EmptyResult
        Right [result] -> Right result 
        Right xs -> Left (MultipleResults xs)