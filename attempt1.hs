import Control.Monad.State

-- We want a program that takes a stream of events and outputs a
-- stream of events. Note that we could have said a stream of key
-- events or even of input events but we want our program to operate
-- as generally as possible, indeed on a timed sequence of any sort of
-- event. Now that we have decided we want a program that maps a
-- stream to a stream, we ask "what is a stream?" and "how is a stream
-- represented?" The former we have already loosely answered: a stream
-- is a sequence of objects. The word sequence implies that these
-- objects arrive in some order, in addition to which we can also
-- impose an index so as to map the ordered arrival of these objects
-- to points in time. So as to further increase the generality of the
-- program, we will assume that these timestamps bear information, and
-- so an event will consist both of an object and its timestamp rather
-- than the object alone. Such a representation allows a sequence of
-- objects, i.e. a list, to be trivially converted to a
-- pseudo-timestamp representation with `flip zip [1..]`. Under this
-- representation, lists may be constructed in which the order of the
-- events (i.e. arrival of objects, i.e. their list index) may not
-- match their accompanying timestamps, e.g. [('a',2),('b',1)]. We
-- will therefore assume that list indices are non-decreasing. We
-- could have said strictly increasing, but we are operating under a
-- latent assumption that only one event can occur at any one time,
-- meaning that if a mental model allows for multiple events to occur
-- at once, this effectively means that their order with respect to
-- one another is irrelevant and therefore we can impose our own order
-- on them, namely the one defined by their order in the list.

-- Our most fundamental entity is that of an event, i.e. a timed object:
type Event a = (a,Time)

-- For our temporary purposes, we will use a simple integer timestamp:
type Time = Int

-- An example, finite, stream of events:
stream = [('f',1),('g',2),('h',3)] :: [Event Char]

-- We want to expose to the user an infinitely configurable system
-- through which to describe remapping behavior. For want of
-- mathematical proof that our exposed API can be used to implement
-- any computable remapping, we will, for now, aim to make it as
-- general as possible.

-- Our exposed API will allow the user to define functions from the
-- set of events to the set of events that can be composed much as
-- layers can be composed. Ultimately, the user will have specified a
-- single function that will then be folded over the sequence of
-- events to continually obtain partial output in the form of output
-- events.

-- As a simple example, we can define an "unmap" (like unknot) that
-- just passes its input on as output:
type Map a = Event a -> Event a

unmap :: Map a
unmap = id

config :: Map a -> Event a -> Map a
config _ _ = id

data FuncCtx a = Ctx [a] (Maybe (a -> a))
data FuncTree a = Node (FuncCtx a) [FuncTree a]
data FTCrumb a = FTCrumb (FuncCtx a) [FuncTree a] [FuncTree a]
type FTZipper a = (FuncTree a, [FTCrumb a])

--ftApply :: a -> FTZipper a -> FTZipper a
--ftApply x (Node (Ctx as (Just f)), cs) = 

funcTreeFold :: FuncTree a -> a -> FuncTree a
funcTreeFold (Node f []) x = Node (Ctx [] (Just id)) []
funcTreeFold (Node f c)  x = Node (Ctx [] (Just id)) []

type Pool a = [Event a]
data Layer a = L (Pool a)

type Expiration a = a -> Bool

-- attributes of a layer: needs to be able to add a layer, possibly
-- needs to have an expiry time (it can check timestamps of events and
-- remove itself once timestamp exceeds its expiry), it needs to be
-- able to remove itself. Removal is predicated on a sequence of
-- events, even just one: for example if the time of an event exceeds
-- a threshold. a layer also needs to be able to hold events to
-- release later. It needs to keep track of some level of history for
-- matching against. 08001114000

-- A layer could contain arbitrarily many predicates that, each time
-- the layer receives new input, match the updated list of recent
-- presses and perform some arbitrary computation such as removing the
-- layer or passing new events on to children layers

-- Let's make a simple data structure consisting of a list structure
-- where each node is able to receive and pass along input, variably
-- removing itself from the list if the input matches some predicate:

{-
baseLayer = do
  x <- getNew
  case
-}

data List a = Done | List (a -> Bool) (List a)

instance Show (List a) where
  show = show . helper
    where
      helper Done = 0
      helper (List _ l) = 1 + helper l

send :: List a -> a -> List a
send Done _ = Done
send (List p l) x = let l' = send l x
                      in if p x then l' else (List p l')



data KList a = Null | KList (State [a] (KList a))

entry :: KList a -> a -> KList a
entry Null _ = Null
entry (KList s) x = fst $ runState s x 

data Node a s = Node
  { state     :: s
  , expiry    :: ((a,s) -> Bool)
  , valuesOut :: (a -> [(Int,[a])])
  , layersOut :: (a -> [Wrapper a s])
  , kids      :: [Wrapper a s]
  }
  
--  Applicator;

--tnrDam :: a -> (a -> a, a

--layerSwitch :: Map a -> Event a -> Map a
--layerSwitch m e = if unmap == id then id else (config m e)

type Func a b = a -> b

{-
newtype Block a = Block { getBlock :: a }
instance Monad Block where
  return x = 

newtype Series a = Series { getSeries :: a -> a }
newtype Parallel a = Parallel { getParallel :: a -> a }

instance Functor Series where
  fmap = 
  
instance Monad Series where
  return = Series
  f >>= Series g = Series $ f . g
  fail _ = Series id

instance Monad Parallel where
  return f = Parallel f
  _ >>= _ =
  fail _ = Parallel id

-- Applying the scheme to keys
type Scancode = (Int,Int)
type KeyMonad = Stream j

-- Two valid types:
-- newtype Foo a = F (a -> ([a], Foo a))
-- newtype Func a = F { getFunc :: a -> Func a }

-- Implicit in our model is the assumption that raw experience (the
-- Tao?) can be separated into objects and time. Further implicit is
-- the assumption that this "time" can be

applier :: Func a -> a -> Func a
applier b x = getFunc b x

fix :: (t -> t) -> t
fix f = f (fix f)

temp :: (a -> a) -> a -> (a -> a)
temp f a = f

addX :: (Num t) => (t -> t) -> t -> t -> t
addX f x = (\e -> (f e) + x)

--ffix :: (t -> t) -> Func t
--ffix f = f (ffix f)

-}
