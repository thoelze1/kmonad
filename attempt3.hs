data Layer a c = Layer
  { ctx  :: c
  , die  :: (a,c) -> Bool
  , emit :: (a,c) -> [a]
  , gen  :: (a,c) -> [Layer a c]
  , ctx' :: (a,c) -> c
  -- IO goes here
  }

instance Show c => Show (Layer a c) where
  show = show . ctx

-- No record syntax for types with multiple constructors
data Tree a c
  = Empty
  | Tree (Layer a c) (Tree a c)

instance Show c => Show (Tree a c) where
  show Empty = ""
  show (Tree l t) = show l ++ "\n" ++ show t

type T a = Tree a [a]

updateCtx :: Layer a c -> a -> Layer a c
updateCtx (Layer c d e g c') x =
  Layer (c' (x,c)) d e g c'

insert :: [Layer a c] -> Tree a c -> Tree a c
insert ls t = foldr Tree t ls

send :: T a -> a -> T a
send Empty _ = Empty
send (Tree l t) x = if die l v
  then t
  else Tree l' t''
  where
    v = (x,ctx l)
    l' = updateCtx l x
    t' = insert (gen l v) t
    t'' = foldl send t' (emit l v)

printIdTree :: IO ()
printIdTree = do
  print t'
    where
      live = const False
      null = const []
      out = Layer [] live null null (\(x,c) -> x:c)
      clear = Layer [] live (\(x,_) -> [x]) null null
      t = Tree clear (Tree out Empty)
      t' = foldl send t ['a','b','c']

printMapTree :: IO ()
printMapTree = do
  print t'
    where
      live = const False
      null = const []
      out = Layer [] live null null (\(x,c) -> x:c)
      remap :: (Char,[Char]) -> [Char]
      remap ('a',_) = ['b']
      remap (c,_) = [c]
      dummy = Layer [] live remap null null
      t = Tree dummy (Tree out Empty)
      t' = foldl send t ['a','b','c']

printModTree :: IO ()
printModTree = do
  print t'
    where
      live = const False
      null = const []
      out = Layer [] live null null (\(x,c) -> x:c)
      remap :: (Char,[Char]) -> [Char]
      remap ('a',_) = ['b']
      remap (c,_) = [c]
      trigger = Layer
                []
                live
                (\(c,l) -> if c == 'x'
                  then []
                  else [c])
                (\(c,l) -> if c == 'x'
                  then [Layer
                       []
                       ((==) 'y' . fst)
                       remap
                       null
                       null]
                  else [])                                            
                null
      t = Tree trigger (Tree out Empty)
      t' = foldl send t ['a','b','c','x','a','a','a','a','y','x','y']

-- if it dies, it doesn't need to return context... so we could return
-- a Maybe Context instead of explicitly returning a Bool. In this
-- model, Nothing would represent removing the layer and Just x would
-- represent updating context

-- We can replace the above code with state monad! instead of
-- explcitly tracking and passing context!
