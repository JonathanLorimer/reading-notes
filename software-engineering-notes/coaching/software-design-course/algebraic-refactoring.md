# Algebraic Refactoring

Instances: [Catalog of Refactoring](https://refactoring.com/catalog/)

## Steps for Algebraic Refactoring

- Reductions
    - Equivalence: Programs are equivalent if `A` can be re-written to `B` through a series of forward and backwards reductions
    - Substitution: Instantiating variables to values
    - Add Parameter: Transform concrete value to a variable (Special case of the un-substitution rule)
    - Extract Method: Factor out a phrase from a command (Special case of the un-substitution rule)
- Algebraic Data Types
    - Distributive law allows refactoring over sums and products

## Equational Reasoning

- Changing a function call
```
((x, y) -> x + y + 1)(a + 1, b)
-------------------------------
(a + 1) + b + 1                 - Substitution
a + b + 1 + 1                   - Communtativity of addition
a + (b + 1) + 1                 - Associativity of addition
((x, y) -> x + y + 1)(a, b + 1) - Un-substitution
-------------------------------
((x, y) -> x + y + 1)(a, b + 1)
```

- Swapping an if-statement
```
if (x) a else b
------------------------------------
((x) -> if (x) a else b)(True)  -> a
((x) -> if (x) a else b)(False) -> b  - Substitution
((x) -> if (!x) b else a)(True)  -> a - !True -> False
((x) -> if (!x) b else a)(False) -> b - !False -> True
if (!x) b else a                      - Un-substitution
------------------------------------
if (!x) b else a
```

- Un-nesting if-statements

```
x => (y => a \/ b) \/ b

if (x) { if (y) a else b  } else b
------------------------------------
((x) -> if (x) { if (True) a else b  } else b) (True) -> a
((x) -> if (x) { if (False) a else b  } else b) (True) -> b
((x) -> if (x) { if (True) a else b  } else b) (False) -> b
((x) -> if (x) { if (False) a else b  } else b) (False) -> b - Substitution
-
((x) -> if (x) { a } else b) (True) -> a
((x) -> if (x) { b } else b) (True) -> b
((x) -> if (x) { a } else b) (False) -> b
((x) -> if (x) { b } else b) (False) -> b - if (True) a else b -> a / if (False) a else b -> b
-
((x) -> if (x) a else b )(True) -> a
((x) -> if (x) a else b )(False) -> b - if (True) b else b === if (False) b else b
((x) -> if (x) a else b )(False) -> b
((x) -> if (x) a else b )(False) -> b - if (False) b else b === if (True) a else b
-
((x) -> if (x) a else b )(True) -> a
((x) -> if (x) a else b )(False) -> b - if (False) b else b === if (False) a else b
((x) -> if (x) a else b )(False) -> b
((x) -> if (x) a else b )(False) -> b
-
((x) -> if (x) a else b )(True && True) -> a
((x) -> if (x) a else b )(True && False) -> b
((x) -> if (x) a else b )(False && True) -> b
((x) -> if (x) a else b )(False && False) -> b - Equivalence
-
if (x && y) a else b                           - Un-substitution
------------------------------------
if (x && y) a else b
```
```
if (x) a else { if (y) a else b }
------------------------------------
((x) -> if (x) a else { if (True) a else b  }) (True) -> a
((x) -> if (x) a else { if (False) a else b  }) (True) -> a
((x) -> if (x) a else { if (True) a else b  }) (False) -> a
((x) -> if (x) a else { if (False) a else b  }) (False) -> b - Substitution
-
((x) -> if (x) a else a) (True) -> a
((x) -> if (x) a else b) (True) -> a
((x) -> if (x) a else a) (False) -> a
((x) -> if (x) a else b) (False) -> b - if (True) a else b -> a / if (False) a else b -> b
-
((x) -> if (x) a else b )(True) -> a - if (True) a else a === if (True) a else b
((x) -> if (x) a else b )(True) -> a
((x) -> if (x) a else a )(True) -> a - if (False) a else a === if (True) a else a
((x) -> if (x) a else b )(False) -> b
-
((x) -> if (x) a else b )(True) -> a
((x) -> if (x) a else b )(True) -> a
((x) -> if (x) a else a )(True) -> a - if (True) a else a === if (True) a else b
((x) -> if (x) a else b )(False) -> b
-
((x) -> if (x) a else b )(True || True) -> a
((x) -> if (x) a else b )(True || False) -> a
((x) -> if (x) a else b )(False || True) -> a
((x) -> if (x) a else b )(False || False) -> b - Equivalence
-
if (x || y) a else b                          - Un-substitution
------------------------------------
if (x || y) a else b
```

- Conditional to Function
```
if (A) o.foo() else o.bar()
------------------------------------
f = if (A) o.foo() else o.bar(); A -> [True/F]
-
if (A) o.foo() else o.bar()
f = if ((A) o.foo() else o.bar())()
f = if (A) (() -> o.foo()) else (() -> o.bar())
f()
-
------------------------------------
f = if (A) (() -> o.foo()) else (() -> o.bar());
f()
```

- Mechanical Refactoring Drill

```
datatype ValidInput = S(dueDate)
                    | S(description)
                    | S(Status)
                    | S(colour)
                    | S(isPublic)
ValidInput * value

S(dueDate) * value + S(description * value) ...
```
```
class ToDoItem:
  def __init__(self, dueDate, description, status, colour, isPublic):
    self.dueDate = dueDate
    self.description = description
    self.status = status
    self.colour = colour
    self.isPublic = isPublic


  def updateDueDate(self, value):
    setattr(self, "dueDate", value)
  def updateDescription(self, value):
    setattr(self, "description", value)
  ...
```
