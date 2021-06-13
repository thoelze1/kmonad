import StreamXForm
import Control.Monad.State

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

modatob :: Manip Char
modatob = modTrigger 'x' (listRemapWithKill ['a'] ['b'] 'y')

main :: IO ()
main = do
  t <- interact' (return (manipsToTree [modatob, out]))
  print t
