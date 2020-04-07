# Performance of Free Monads

# Church Encodings
- re-defines a data type in terms of its fold function
```haskell
data EList a = EList (forall b. (a -> b -> b) -> b -> b)

toEList :: [a] -> EList a
toEList xs = EList (\el acc -> foldr el acc xs)

fromEList :: EList a -> [a]
fromEList (EList fold) = fold (:) []
```
- Church encodings work well for composing fold functions, but can have performance hinderances (i.e. `tail` for EList)

# Scott Encodings
- re-defines a data type in terms of pattern matching
- The implementation in the book is tied to the list type (by the second argument in the callback) but can be recursive
```haskell
data SList a = SList (forall b. (a -> [a] -> b) -> b -> b)

toSList :: [a] -> SList a
toSList [] = SList $ \_ z -> z
toSList (x:xs) = SList $ \f _ -> f x xs

fromSList :: SList a -> [a]
fromSList (SList f) =  f (:) []
```

# Codensity
- Allows us to avoid the left nesting of binds
```haskell
data Codensity m a = Codensity { runCodensity :: forall b. (a -> m b) -> m b }
```

# Type Aligned Sequences
- These allow for interleaving of inspection with building computations, this provides the opportunity to optimize as the effects are built
```
    b -> m d
     /    \
    /      \
   /        \
a -> m b  b -> m c
```
