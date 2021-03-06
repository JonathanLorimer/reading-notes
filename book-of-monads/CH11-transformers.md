# 11. A Solution: Monad Transformers

Tags:[ #haskell, #monads, #monad-transformers ]
@ID:20200229132720

## Reference
1. [[20200229134145]]Monad transformers solve the 'composition of monads' problem by creating a 'super monad' from constituent smaller monads.
2. [[20200301184415]]Monad transformers are constructed from a polymorphic monad wrapping a concrete monad.
3. [[20200301205515]]A monad transformer is not itself a monad until it is applied to another monad type.
4. [[20200301211131]]The `ListT` monad transformerbased on the underlying `[]` type does not always obey the monad laws.
5. [[20200301212102]]We can nest transformers to create monad stacks.
6. [[20200301214020]]Monad stacks behave differently when their internal order is switched.
7. [[20200301215939]]We can use type-classes to thread the correct functions through our program based on our monad stack, this is known as`MTL style`
8. [[20200301223208]]IO must always be at the base of the stack, we can use `liftIO` to run an IO action from anywhere in the stack.
9. [[20200301224548]]Some monad transformers have functional dependencies which require special considerations when applying multiple constraints.
10. [[20200301225808]]There are 3 layers to monad transformers: concrete monads, monad transformers, transformer typeclasses.
## Literature Notes
1. @ID:20200229134145 (pg. 133): Monad transformers solve the 'composition of monad' problem naïvely. A new `bind` and `return` implementation is created based on the specific monads that make up monad stack; each layer of the monad stack has a `bind` and `return`, and these are used to create a `bind` and `return` for the whole stack. This means that each layer of the stack is a monad, but the stack in its entierity is also a monad.
2. @ID:20200301184415 (pg. 134): A monad transformer is constructed from a concrete monad and a polymorphic value which can be occupied by any other monad. For example: 

```haskell
data MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
``` 
3. @ID:20200301205515 (pg.135): A monad transformer is not actually a monad until it is applied to a monad; it has the wrong kind. The kind of a monad is `* -> *` the kind of a monad Transformer is `(* -> *) -> * -> *` , it only becomes a monad once it is applied down to `* -> *`. For example:

```haskell
:k MaybeT    -- (* -> *) -> * -> *
:k MaybeT IO --             * -> *
```
4. @ID:20200301211131 (pg. 137): The List monad is complicated because the naive implementation `ListT m a = m [a]` only behaves for commutative monads. There are several impementations that respect the monad laws and interleave effects correctly. For example:
			
```haskell
-- | in list-transformer
newtype ListT m a = ListT { next :: m (Step m a)}
data    Step  m a = Cons a (ListT m a) | Nil
```

```haskell
-- | in list-t
newtype ListT m a = ListT (m (Maybe (a, ListT m a)))
```
5. @ID:20200301212102 (pg. 140): We can create `monad stacks` by layering transformers after one another. The most deeply nested monad is called the _bottom layer_ and the outermost monad is called the _top layer_. For example:

```haskell
ReaderT Environment (WriterT [Message] (MaybeT Identity)) Person
-- ReaderT is the top layer
-- MaybeT Identity or Maybe is the bottom layer
```
6. @ID:20200301214020 (pg.141): Because monads are not commutative their order matters. This can be observed from the difference in these two monads once they are executed:

```haskell
ReaderT Environment (WriterT [Message] Maybe) Person
-- Environment -> Maybe (Person, [Message])

ReaderT Environment (MaybeT (WriterT [Message])) Person
-- Environment -> (Maybe Person, [Message])
```
7. @ID:20200301215939 (pg. 143): The MTL style type-class implementation of monadic behaviour allows us to get the functionality we want out of a monad stack without specifying the exact stack, we just put constraints on our stack i.e `MonadPlus`, `MonadError`, `MonadState`, `MonadReader`, and `MonadWriter`
8. @ID:20200301223208 (pg. 144): IO does not have a monad transformer because it cannot ever wrap another monad transformer, but it can be wrapped by other transformers. Therefore IO must always be our _base monad_. We do get a type class `MonadIO` with the `liftIO` function which allows us to run an IO action in our monad stack, regardless of where we are.
9. @ID:20200301224548 (pg.146): Some MTL style typeclasses have what is called a functional dependency `| m -> r`. This means that the type of `m` determines the type of `r`. This is necessary because the return type of `MonadReader` is just `m`. This prevents us from having multiple `MonadReader` constraints on the same `m` type. There are several solutions to this, however the most popular can be found in [Classy Lenses](<https://hackage.haskell.org/package/lens-4.16/docs/Control-Lens-TH.html#v:makeClassy>) and [Ether](<https://hackage.haskell.org/package/ether>).
10. @ID:20200301225808 (pg.147): There are three layers of functionality when it comes to monad transformers. The basic monads such as `Reader` and `Maybe` which provide a return and bind i.e. basic monadic functionality. The monad trasnformers such as `ReaderT` and `MaybeT`which allow us to nest monads and achieve a bind and return based on the underlying concrete monad. The typeclasses which give us the functionality we expect, across different monad stacks i.e. `MonadReader` and `ask`. Basic concrete monads can be found in base and [transformers](<https://hackage.haskell.org/package/transformers>), monad transformers can also be found in the transformers package, and the typeclasses can be found in [mtl](<https://hackage.haskell.org/package/mtl>) and [ether](<https://hackage.haskell.org/package/ether>). Simply importing _mtl_ is the most common solution.