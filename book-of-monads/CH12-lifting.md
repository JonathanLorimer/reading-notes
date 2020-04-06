# 12. Generic Lifting and Unlifting

Tags:[ #haskell, #monads, #monad-transformers ]
@ID:20200304101345


## Reference
1. [[20200304102246]]`mtl` style monad transformers experience an n^2 problem for instances.
2. [[20200304103701]]Specific monads can lift other monadic operations across themselves in a generic way
3. [[20200304104804]]While elegant, the MonadTrans solution pollutes the instance space.

## Literature Notes
1. @ID:20200304102246 (pg.151-153): The n^2 problem is a symptom of the fact that monads do not compose generically; for an option like the `mtl` typeclass solution one has to write n^2 instances where n is the number of monads one must accomodate. This problem highlights that fact that monad composition requires knowledge of the specific monad and its behaviour. This problem is made even more obvious when we have to move operations across layers of a monad stack
2. @ID:20200304103701 (pg. 154): We can write a generic way to lift monadic operations for a specific monad by creating another typeclass with the following operation `lift :: Monad m => m a -> t m a`. This solution identifies the property that specific monads compose in a consistent way. Therefore we don't need to write the cross product of operation instances, due to the fact that every monad needs to support the operations of every other monad, but rather that each monad needs to provide an interface for lifting across its specific structure. Example:

```haskell
instance MonadTrans MaybeT where
			    lift x = MaybeT $ Just <$> x
instance MonadTrans ReaderT where
    lift x = ReaderT $ \_ -> x
```
3. @ID:20200304104804 (pg. 155): The MonadTrans solution creates ambiguous instances for the same monad stack, and therefore the `mtl` library is implemented using concrete monads. In practice `lift` is not used to implement monad instances, but to inject monadic operations into larger stacks.
4. @ID:20200304113022 (pg. 156): Base monads must exist at the bottom of the transformer stack, put another way they do have transformer instances. Examples of base monads are `IO`, `ST` and `STM`
5. @ID:20200304113633 (pg. 157): `lift` as a solution only changes the monad for the return type i.e. in the covariant position. to change the monad in the parameter type / return type of a callback / contravariant position we need to use something called `unlift` 