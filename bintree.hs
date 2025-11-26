import qualified Data.Map as Map

data BinTree a
    = Nil
    | Node a (BinTree a) (BinTree a)

binTreeMap :: (a -> b) -> BinTree a -> BinTree b 
binTreeMap f Nil = Nil 
binTreeMap f (Node a left right) =
    Node (f a) (binTreeMap f left) (binTreeMap f right)

tree :: BinTree String
tree = Node "Eins"
            (Node "Zwei" 
                (Node "Vier" Nil Nil)
                (Node "Fuenf" Nil Nil))
            (Node "Drei"
                (Node "Sechs" Nil Nil)
                (Node "Sieben" Nil Nil))


treeTraverseD :: (b -> a -> b) -> b -> BinTree a -> b 
treeTraverseD f acc Nil = acc 
treeTraverseD f acc (Node val left right) =
    let leftResult = treeTraverseD f acc left 
    in treeTraverseD f (f  leftResult val) right


treeTraverseW :: (b -> a -> b) -> b -> BinTree a -> b 
treeTraverseW f acc Nil = acc 
treeTraverseW f acc node = helper f acc [node] where 
    helper f acc [] = acc
    helper f acc (Nil:ns) = helper f acc ns
    helper f acc ((Node val left right):ns) = 
        helper f (f acc val) (ns ++ [left, right])

data BinOp 
    = Add
    | Sub
    | Div
    | Mul
    deriving (Eq)

binOpToSign :: BinOp -> String
binOpToSign Add     = "+"
binOpToSign Sub    = "-"
binOpToSign Div   = "/"
binOpToSign Mul = "*"

calcBinOp :: BinOp -> Double -> Double -> Double 
calcBinOp Add = (+)
calcBinOp Sub = (-)
calcBinOp Div = (/)
calcBinOp Mul = (*)

data UnOp 
    = Sin 
    | Cos 
    | Tan
    | Sqrt
    deriving (Eq)

unOpToSign :: UnOp -> String
unOpToSign Sin = "sin"
unOpToSign Cos = "cos" 
unOpToSign Tan = "tan"
unOpToSign Sqrt = "sqrt"

calcUnOp :: UnOp -> Double -> Double 
calcUnOp Sin = sin 
calcUnOp Cos = cos 
calcUnOp Tan = tan
calcUnOp Sqrt = sqrt

data ExprTree 
    = Value Double
    | Variable String
    | BinNode BinOp ExprTree ExprTree
    | UnNode UnOp ExprTree

algbTree :: ExprTree
algbTree = BinNode Add
            (Value 5)
            (BinNode Sub
                (BinNode Div 
                    (BinNode Mul 
                        (Variable "y")
                        (BinNode Add 
                            (Value 6)
                            (Value 2)))
                    (Value 3)) 
                (UnNode Sin
                    (Variable "x")))

exprTreeToString :: ExprTree -> String
exprTreeToString t = dfs t where
    dfs :: ExprTree -> String
    dfs (Value x) = show x
    dfs (Variable var) = var
    dfs (BinNode binOp left right) = 
        let leftResult = dfs left
            rightResult = dfs right
            leftWithParens = if needParensLeft binOp left then "(" ++ leftResult ++ ")" else leftResult
            rightWithParens = if needParensRight binOp right then "(" ++ rightResult ++ ")" else rightResult 
        in leftWithParens ++ " " ++ binOpToSign binOp ++ " " ++ rightWithParens
    dfs (UnNode unOp expr) = 
        (unOpToSign unOp) ++ "(" ++ dfs expr ++ ")"

type Params = [(String, Double)]

calcTree :: ExprTree -> Params -> Double
calcTree (Value x) ps = x 
calcTree (Variable var) ps = 
    case lookup var ps of
        Just val -> val
        Nothing -> error $ "didn't give value for variable " ++ var
calcTree (BinNode binOp left right) ps = 
    calcBinOp binOp (calcTree left ps) (calcTree right ps)
calcTree (UnNode unOp expr) ps = 
    calcUnOp unOp (calcTree expr ps)



-- вычислить константы
-- заменить 0 + t, 1 * t   на t
--          0 * t          на 0
--          t - t          на 0 и тд

simplifyTree :: ExprTree -> ExprTree
simplifyTree (Value x) = Value x
simplifyTree (Variable var) = Variable var
simplifyTree (UnNode unOp expr) = 
    let simplifiedExpr = simplifyTree expr
    in case simplifiedExpr of
        Value x -> Value (calcUnOp unOp x)
        _       -> UnNode unOp simplifiedExpr

simplifyTree (BinNode binOp left right) = 
    let simplifiedLeft = simplifyTree left 
        simplifiedRight = simplifyTree right
    in case (simplifiedLeft, simplifiedRight) of
        (Value l, Value r) -> (Value (calcBinOp binOp l r))

        (_, Value 0) | binOp == Add -> simplifiedLeft           -- x + 0 = x
        (Value 0, _) | binOp == Add -> simplifiedRight          -- 0 + x = x
        (_, Value 0) | binOp == Sub -> simplifiedLeft          -- x - 0 = x  
        (_, Value 1) | binOp == Mul -> simplifiedLeft       -- x * 1 = x
        (Value 1, _) | binOp == Mul -> simplifiedRight      -- 1 * x = x
        (Value 0, _) | binOp == Mul -> Value 0              -- 0 * x = 0
        (_, Value 0) | binOp == Mul -> Value 0              -- x * 0 = 0
        (_, Value 1) | binOp == Div -> simplifiedLeft         -- x / 1 = x
        (Value 0, _) | binOp == Div -> Value 0                -- 0 / x = 0
        (Variable v1, Variable v2) 
                |binOp == Sub && v1 == v2 -> Value 0           -- x - x = 0

        _ -> BinNode binOp simplifiedLeft simplifiedRight

diffTree :: ExprTree -> String -> ExprTree
diffTree (Value _) _ = Value 0
diffTree (Variable var) diffVar 
    | var == diffVar    = Value 1
    | otherwise         = Value 0
diffTree (BinNode binOp left right) diffVar = 
    case binOp of 
    Add -> BinNode Add (diffTree left diffVar) (diffTree right diffVar)
    Sub -> BinNode Sub (diffTree left diffVar) (diffTree right diffVar)
    Mul -> (BinNode Add
                    (BinNode Mul
                        (diffTree left diffVar)
                        (right))
                    (BinNode Mul
                        (left)
                        (diffTree right diffVar)))
    Div -> (BinNode Div
                    (BinNode Sub
                        (BinNode Mul
                            (diffTree left diffVar)
                            (right))
                        (BinNode Mul
                            (left)
                            (diffTree right diffVar)))
                    (BinNode Mul
                        (right)
                        (right)))
diffTree (UnNode unOp expr) diffVar =
    case unOp of
    Sin -> BinNode Mul 
            (UnNode Cos expr)
            (diffTree expr diffVar)
    Cos -> BinNode Mul 
            (BinNode Mul 
                (Value (-1))
                (UnNode Sin expr))
            (diffTree expr diffVar)
    Tan -> BinNode Mul 
            (BinNode Div 
                (Value 1)
                (BinNode Mul 
                    (UnNode Cos expr)
                    (UnNode Cos expr)))
            (diffTree expr diffVar)
    Sqrt -> BinNode Div 
                (Value 1)
                (BinNode Mul
                    (Value 2)
                    (UnNode Sqrt expr))


-- functions for parenthesis
needParensLeft :: BinOp -> ExprTree -> Bool
needParensLeft Mul (BinNode Add _ _) = True
needParensLeft Mul (BinNode Sub _ _) = True
needParensLeft Div (BinNode Add _ _) = True
needParensLeft Div (BinNode Sub _ _) = True
needParensLeft _ (UnNode _ _) = True 
needParensLeft _ _ = False

needParensRight :: BinOp -> ExprTree -> Bool
needParensRight Mul (BinNode Add _ _) = True
needParensRight Mul (BinNode Sub _ _) = True
needParensRight Div (BinNode Add _ _) = True
needParensRight Div (BinNode Sub _ _) = True
needParensRight Sub (BinNode Add _ _) = True      
needParensRight Sub (BinNode Sub _ _) = True    
needParensRight Div (BinNode Mul _ _) = True 
needParensRight Div (BinNode Div _ _) = True  
needParensRight _ _ = False



-- деревья для тестов
testTree :: ExprTree
testTree = BinNode Add
    (BinNode Add (Value 2) (Value 3))                    
    
    (BinNode Sub
        (BinNode Mul 
            (BinNode Div 
                (BinNode Mul (Value 4) (Value 5))   
                (Value 2))                             
            (BinNode Sub (Value 8) (Value 3)))         
        
        (BinNode Div
            (BinNode Add
                (Variable "x")
                (Value 0))                               
            
            (BinNode Mul
                (Variable "y") 
                (Value 1)))                              
    )

edgeCaseTree :: ExprTree  
edgeCaseTree = BinNode Mul
    (BinNode Add
        (BinNode Mul (Value 0) (Variable "a"))       
        (BinNode Mul (Variable "b") (Value 0))      
    )
    (BinNode Add
        (BinNode Sub
            (UnNode Sin (Value 0))                      
            (BinNode Div (Variable "z") (Value 1))     
        )
        (BinNode Sub (Variable "w") (Variable "w"))    
    )

mixedTree :: ExprTree
mixedTree = BinNode Add
    (Value 5)
    (BinNode Sub
        (BinNode Div 
            (BinNode Mul 
                (Variable "y")
                (BinNode Add (Value 6) (Value 2)))      
            (Value 3)) 
        (UnNode Sin (Variable "x"))
    )

precedenceTree :: ExprTree
precedenceTree = BinNode Mul
    (BinNode Add
        (Value 1)
        (BinNode Mul
            (Value 0)
            (Variable "p")                               
        )
    )
    (BinNode Div
        (BinNode Sub
            (Variable "q")
            (Value 0)                                     
        )
        (Value 1)                                      
    )                                                     


megaTree :: ExprTree
megaTree = 
    BinNode Add
        (Variable "x")
        
        (BinNode Add
            (BinNode Mul
                (BinNode Mul (Value 2) (Variable "x"))
                (Variable "y"))
            
            (BinNode Sub
                (BinNode Div
                    (UnNode Sin (Variable "x"))
                    (Variable "x"))
                
                (UnNode Cos
                    (UnNode Tan (Variable "x"))))
        )
