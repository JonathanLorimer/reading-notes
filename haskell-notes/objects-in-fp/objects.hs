{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}

import Data.IORef

data Shape a = Shape { getX    :: IO a
                     , getY    :: IO a
                     , setX    :: a -> IO ()
                     , setY    :: a -> IO ()
                     , moveTo  ::  a -> a -> IO ()
                     , rMoveTo ::  a -> a -> IO ()
                     }

-- data Q a = forall m. Quasi m => Q { unQ :: m a }

shape :: Int -> Int -> IO (Shape Int)
shape newx newy = do
  x <- newIORef newx
  y <- newIORef newy

  let obj = Shape { getX    = readIORef x
                  , getY    = readIORef y
                  , setX    = writeIORef x
                  , setY    = writeIORef y
                  , moveTo  = \newx' newy' -> do
                      setX obj newx'
                      setY obj newy'

                  , rMoveTo = \deltax deltay -> do
                      x <- getX obj
                      y <- getY obj
                      moveTo obj (x + deltax) (y + deltay)
                  }

  return obj

rectangle :: Int -> Int -> Int -> Int -> IO (Shape Int)
rectangle x y width height = do
  Shape { getX
        , getY
        , setX
        , setY
        , moveTo
        , rMoveTo
        }  <- shape x y
  w <- newIORef width
  h <- newIORef height

  let rec = Rectangle { getWidth = readIORef w
                      , getHeight = readIORef h
                      , setWidth = (\neww -> writeIORef w neww)
                      , setHeight = (\newh -> writeIORef h newh)
                      , draw = do
                          putStr $ "Drawing a Rectangle at: "
                          ++ "x: " ++ (show << getX) ++ ","
                          ++ "y: " ++ (show << getY) ++ ","
                          ++ "width: " ++ (show << getWidth) ++ ","
                          ++ "height: " ++ (show << getHeight) ++ ","
                      , getX = getX
                      , getY = getY
                      , getX = getX
                      , setY = setY
                      , moveTo = moveTo
                      , rMoveTo = rMoveTo
                      }

  shape 100 100


myShape :: IO (Shape Int)
myShape = shape 100 100

main :: IO ()
main = do
  -- initialize
  s <- myShape
  x <- getX s
  y <- getY s

  putStrLn ("x: " ++ show x)
  putStrLn ("y: " ++ show y)

  -- test moveTo
  moveTo s 20 20
  x' <- getX s
  y' <- getY s

  putStrLn ("x': " ++ show x')
  putStrLn ("y': " ++ show y')

  -- test rMoveTo
  rMoveTo s 5 5
  x'' <- getX s
  y'' <- getY s

  putStrLn ("x'': " ++ show x'')
  putStrLn ("y'': " ++ show y'')

-- $> main
