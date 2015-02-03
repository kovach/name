module Name
  ( Name, NameMonad, EnvMonad, Env
  , fresh, store, get
  , names, values
  , runEnv, evalEnv, runName
  -- Only these mutate:
  , modify, put
  , debug
  ) where
import Control.Monad.State hiding (get, put, modify)
import qualified Control.Monad.State as S (get, put, modify)
import qualified Data.Map as M

newtype Name = N Int
 deriving (Eq, Ord)
instance Show Name where
  show (N n) = "#"++show n

type NameMonad = State Int
type Env a = M.Map Name a
type EnvMonad a = State (Int, Env a)

runName :: NameMonad a -> a
runName m = evalState m 0

evalEnv :: EnvMonad a b -> b
evalEnv m = evalState m (0, M.empty)

runEnv :: EnvMonad a b -> (b, Env a)
runEnv m =
  let (a, (_, map)) = runState m (0, M.empty)
  in (a, map)

fresh :: NameMonad Name
fresh = do
  n <- S.get
  S.modify (+ 1)
  return (N n)

store :: a -> EnvMonad a Name
store a = do
  (c, m) <- S.get
  S.put (c+1, M.insert (N c) a m)
  return (N c)

modify :: Name -> (a -> a) -> EnvMonad a ()
modify name f = do
  v <- get name
  S.modify (\(c, m) -> (c, M.insert name (f v) m))

put :: Name -> a -> EnvMonad a ()
put name x = do
  modify name (const x)

get :: Name -> EnvMonad a a
get n = gets (fromJust' ("val. error: " ++ show n) . M.lookup n . snd)

values :: EnvMonad a [a]
values = do
  (_, env) <- S.get
  return $ map snd $ M.toList env

names :: EnvMonad a [Name]
names = do
  (_, env) <- S.get
  return $ map fst $ M.toList env


prev :: Name -> Maybe Name
prev (N 0) = Nothing
prev (N n) = Just $ N (n-1)

debug :: EnvMonad a (M.Map Name a)
debug = gets snd

fromJust' _ (Just n) = n
fromJust' str _ = error $ "fromJust. " ++ str
