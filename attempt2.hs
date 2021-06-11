import Control.Monad.State
import Control.Monad.Fail

-- type Layer a = State [a] (a -> a)
-- type Stack a = State [Layer a]

type Stack = [Int]  

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)

-- the starting state is the tnr trigger layers and any default layout
-- that the user defines. the manipulation function is

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    a <- pop  
    pop

type Layer a = State [a] (a -> a)
type Layers a = [Layer a]
type Ctx a = State (Layers a, [a]) a

test :: Int -> Int -> Bool
test i = \x ->
  case () of
    _ | x == i    -> True
      | otherwise -> False

shiftLayer :: State [a] (a -> a)
shiftLayer = state 

tnr x = do
  guard p x

let layer = tnr 'c' 'h' 'x' :: x -> ?

makeBaseLayer :: Char -> Char -> Char -> Char -> State [Layer Char] ()
makeBaseLayer i t h c
  | c == i = do
      ls <- get
      put $ ls ++ (return 
  | otherwise = do
      return ()
          
baseLayer :: Char -> State (Layers Char) ()
baseLayer 'X' = do
  ls <- get
  put ls
baseLayer _ =  do
  ls <- get
  put ls

{-
runMap :: Ctx a
runMap = do
  (ls, a:as) <- get
  return ()
-}
