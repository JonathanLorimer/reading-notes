# Overlapping Input/Output

- Async/Await: We can use `putMVar` and `readMVar` or `takeMVar` to leverage the lock mechanism to handle awaiting asynchronous actions

## Exceptions in Haskell

- Exceptions are thrown by the throw function `throw :: Exception e => e -> a`
- `ErrorCall` is a newtype wrapper around `String` that has an instance of `Typeable` so it is a basic candidate for an exception type
- `error` from prelude just takes a string and throws it wrapped in an `ErrorCall`
- *you can only catch exceptions in the IO monad* `catch :: Exception e => IO a -> (e -> IO a) -> IO a`
- We need to provide a type for the exception that is being caught i.e. ` `catch` \e -> print (e :: MyException`, exceptions form a hierarchy so you can just use `SomeException` if in doubt
- Prefer `throwIO` in the IO monad rather than `throw` because `throwIO` ensures strict ordering with respect to other IO actions

## Useful Functions
```haskell
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
finally :: IO a -> IO b -> IO a
```
- bracket - `IO a` is a resource `(a -> IO b)` is a function that deallocates that resource `(a -> IO c)` is the operation we wish to perform on the resource
- finally is a special case of bracket


