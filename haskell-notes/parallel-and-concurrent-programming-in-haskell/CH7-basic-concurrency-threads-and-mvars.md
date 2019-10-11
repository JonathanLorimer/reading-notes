# Basic Concurrency: Threads and MVars

- The fundamental building block for concurrent programming in haskell are threads, which can be generated using `forkIO`
```haskell
forkIO :: IO () -> IO ThreadId
```
- Interleaving: effects from different threads will be interleaved in an indeterminate fashion
- Fairness: GHC's fairness policy can cause effects, which are contending for a shared resource, to oscillate
- Termination: the program terminates when `main` returns, even if there are other threads still running; the other threads will be terminated as well. This is just a decision that was made for Haskell.

## Useful Functions
- `threadDelay :: Int -> IO ()` takes a number representing a number of milliseconds to wait before returning

## Communication: MVars

- MVars can be found in `Control.Concurrent.MVar`
- API Operations
    - `newEmptyMVar :: IO (MVar a)` creates a new empty box
    - `newMVar :: a -> IO (MVar a)` creates a new full box containing the value passed
    - `takeMVar :: MVar a -> IO a` removes the value from a full `MVar` and returns it, blocks if the `MVar` is currently empty
    - `putMVar :: MVar a -> a -> IO ()` puts a value into the `MVar` but blocks if the `MVar` is already full
- MVar is a fundamental building block that generalizes communication and synchronization patterns
- In summary
    - A *one place channel* that can be used to pass messages between threads
    - A *container for shared mutable state*, or a representation i.e. a file handle can be represented with `MVar ()`
    - A *building block* for constructing larger concurrent Datastructures

## Use Cases for MVars

- Logging Service

```haskell
import Control.Concurrent
import Control.Monad

data Logger = Logger (MVar LogCommand)

data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
 where
  loop = do
    cmd <- takeMVar m
    case cmd of
      Message msg -> do
        putStrLn msg
        loop
      Stop s -> do
        putStrLn "logger: stop"
        putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

main :: IO ()
main = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "bye"
  logStop l
```

- Shared State
```haskell
import Data.Map
import Control.Concurrent

type Name        = String
type PhoneNumber = String
type PhoneBook   = Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = do
  m <- newMVar Map.empty
  return (PhoneBookState m)

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
  book <- takeMVar m
  putMVar m (Map.insert name number book)

lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return (Map.lookup name book)

main = do
  s <- new
  sequence_ [ insert s ("name" ++ show n) (show n) | n <- [1..10000] ]
  lookup s "name999" >>= print
  lookup s "unknown" >>= print
```


