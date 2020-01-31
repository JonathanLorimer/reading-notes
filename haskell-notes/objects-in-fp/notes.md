# Objects in FP

## What are objects?
[source](https://blog.cleancoder.com/uncle-bob/2019/06/16/ObjectsAndDataStructures.html)
- An Object is a set of functions that operate upon implied data elements.
- A Data Structure is a set of data elements operated upon by implied functions
- Classes make it easy to add types but hard to add functions. Data structures make it easy to add functions but hard to add types.
- Data Structures expose callers to recompilation and redeployment. Classes isolate callers from recompilation and redeployment.

## Inheritance VS Subtyping - James Koppel
- *Subtyping*: suggests that a more specific type may be used when a more general type is expected
- *Inheritance*: the process of defining an object, in terms of, and by modifying an existing object
- *Liskov Substitution Principle*: S is a subtype of T, if, for any function f that expects a value of type T, f will continue to work if instead given a value of type S.


