# 13. Defining Custom Monads

Tags:[ #haskell, #monads, #free-monads ]
@ID:20200304114916


## Reference
1. [[20200304142439]]Initial and Final style encodings apply to any data type, not just monads.
2. [[20200304155254]]The Final Style monad encodes syntax using a typeclass and primitive operations. It encodes semantics through the instances of that typeclass and computations that use these instances.
3. [[20200304164424]]Initial Style monads use data structures to build up a structure (the syntax) that represents the computation, and then interpret it (the semantics).
4. [[20200304221846]]An Initial Style monad has 4 characteristic properties: `Done`, a continuation, smart constructors, and an interpreter.
5. [[20200304223742]]A free monad exists when one factors out the recursion from an initial style data type and uses the `free` data type to provide that separately.
6. [[20200306110553]]A free monad guarantees a bind instance so long as you have a functor instance for the underlying pattern functor.
7. [[20200306121424]]Operational style initial monads encode the `>>=` operation as a data type.

## Literature Notes
1. @ID:20200304142439 (pg. 171): Final and Initial style descriptions of data are not tied directly to Monads and can be used to describe any data type.
2. @ID:20200304155254 (pg.176,177): A Final Style Monad is one that is encoded using a typeclass which is polymorphic over a monad that inhabits it. The typeclass defines the primitive operations associated with the computation that the monad describes. A computation that uses these operations is parameterized by a polymorphic monad with the typeclass as a constraint. The typeclass and the operations define the syntax, instances of the typeclass and computations that use the primitive operations define the semantics. 
3. @ID:20200304164424 (pg. 179): The Initial Style encoding turns the primitive operations into data structures. The data structures have 2 characteristics. 1) Each constructor has a field per argument that the operation would take 2) Each constructor has an additional field with the type `R -> M a` where `R` is the return type of the operation. The reason for the continuation is that it gives us a way to continue the computation. To make initial style data structures a mond `Done` becomes `return` and for `>>=` you enlarge the continuation (by calling the supplied continutation) and recursively calling bind. One can encode helper functions by supplying `return` or `done` as the continuation.
4. @ID:20200304221846 (pg. 184): All initial style monads share 4 things in common: 1) a data type that signals completion 2) every other data type includes a continuation 3) smart constructors to yield a better api / make the interface similar to other monads 4) an interpretation function to other monads (natural transformation).
5. @ID:20200304223742 (pg. 185): When one has an initial style monad they can factor out the return type of the continuation and depend on the `free` data type to provide the recursion.

```haskell
data Free f a = Free (f (Free f a))
																	              | Pure a
```
6. @ID:20200306110553 (pg. 185): For a `Free f a` we are guaranteed that we will have a monad instance so long as the pattern functor `f` is indeed a functor.
7. @ID:20200306121424 (pg. 190): Operational style initial monads add a data constructor for the `>>=` operation. This makes it more apparent how `do` blocks desugar to data types.