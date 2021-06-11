import Control.Monad.State

-- Add a monad as a type parameter to replace the "Maybe" below.
data Layer a c = Layer
  { ctx :: c
  , run :: a -> State (Maybe c) ([a],[Layer a c])
  -- IO goes here
  }

type Manip a = a -> State (Maybe [a]) ([a],[Layer a [a]])

mkLayer :: Manip a -> Layer a [a]
mkLayer = Layer []

instance Show c => Show (Layer a c) where
  show = show . ctx

-- No record syntax for types with multiple constructors
data Tree a c
  = Empty
  | Tree (Layer a c) (Tree a c)

instance Show c => Show (Tree a c) where
  show Empty = ""
  show (Tree l t) = (show l) ++ "\n" ++ (show t)

insert :: [Layer a c] -> Tree a c -> Tree a c
insert ls t = foldr Tree t ls

type T a = Tree a [a]

send :: T a -> a -> T a
send Empty _ = Empty
send (Tree l t) x =
  case s of
    Nothing -> t
    Just c' -> Tree (Layer c' r) t'
  where
    r = run l
    ((xs,ls),s) = runState (r x) (Just $ ctx l)
    t' = foldl send (insert ls t) xs

exec :: T a -> [a] -> T a
exec = foldl send

-- FIXME Because we are invoking the following stateful transofmations
-- from within `send`, it is impossible that the result of `<- get`
-- will ever be of the form `Nothing` (based on the behavior of
-- `send`). Therefore I think using a Maybe context for the state to
-- communicate to `send` whether the layer should be removed or not is
-- a hack.

getMatches :: (Eq a) => [a] -> [b] -> a -> [b]
getMatches xs ys x = snd $ unzip $ filter ((==) x . fst) (zip xs ys)

-- We currently have remap written in terms of listRemap. But is there
-- a way to write listRemap in terms of remap? Perhaps such a way
-- would hint at better ways of combining multiple layers...

listRemap :: (Eq a) => [a] -> [a] -> Manip a
listRemap xs ys c
  | c `elem` xs = do
      return (getMatches xs ys c,[])
  | otherwise = return ([c],[])

remap :: (Eq a) => a -> a -> Manip a
remap x y = listRemap [x] [y]

listRemapWithKill :: (Eq a) => [a] -> [a] -> a -> Manip a
listRemapWithKill xs ys k c
  | c == k = do
      put Nothing
      return ([],[])
  | otherwise = listRemap xs ys c

modTrigger :: (Eq a) => a -> Manip a -> Manip a
modTrigger x m c
  | c == x = do
      return ([],[mkLayer m])
  | otherwise = do
      return ([c],[])

out :: Manip a
out c = do
  mc <- get
  case mc of
    Nothing -> return ([],[])
    Just cs -> do
      put (Just (c:cs))
      return ([],[])

clear :: Manip a
clear c = do
  return ([c],[])

atob :: Manip Char
atob = listRemap ['a'] ['b']

{-
atob :: Manip Char
atob c = do
  case c of
    'a' -> return (['b'],[])
    _ -> return ([c],[])
-}

modatob :: Manip Char
modatob = modTrigger 'x' (listRemapWithKill ['a'] ['b'] 'y')
