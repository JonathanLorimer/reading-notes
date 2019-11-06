# Getting Precise About Language

## 2.1 Mathematical Statements
- Modern mathematical language is concerned with *statements* about *mathematical objects*


## 2.2 The Logical Combinators and, or, and not
- Conjunction (&) is a commutative, associative operation that is false in case any argument is false
- Disjunction (||) is a commuatative, associative operation that is true in case any argument is true
    - Note: contrary to the ambiguous use in english, mathematical or is inclusive
- Negation (-) an operation that changes truth value, be careful when negating existentials

## 2.3 Implication
- Implication (=>) is an operation where the first argument is called the antecedent and the second argument is the consequent
    - Note: implication is only fasifiable when the antecedent is true and the consequent is false
- Biconditional (<=>) expresses equivalents and can be destructured like so `a <=> b ~ a => b & b => a`

## 2.4 Quantifiers
- Existential Quantification: `∃x` means that there is some `x` such that
- Universal Quantification: `∀x` means for every x
- Negation rules:
  - inverse of `∀x Yx` is `∃x ¬Yx`
      - colloquialy: for every x predicate Y applies v.s. there is some x for which predicate Y does not apply
  - inverse of `∃x Yx` is `∀x ¬Yx`
      - colloquially: there is some x for which predicate Y applies v.s. for every x predicate Y does not apply

## 2.1.1 Exercises
1. Come up with a counter-example or contradiction, but there isn't one
2. The woman saw the man holding the telescope, The man saw the woman holding the telescope
3. sentences:
    - Sisters reunited in checkout line at safeway, after 10 years
    - Prostitutes make appeal to the Pope
    - Large hole appears in High Street. City authorities are managing it
    - Mayor recommends safety measures for passengers
4. All head injuries should be attended to
5. There is the natural interpretation, and then the interpretation that use of the elevator will cause fire. Reformulation: Do not use elevator during a fire
6. If the page was left blank then it would not have the warning on it, but if it does not have the warning on it then it appears to be unintentionally left blank. Reformulation: This page acts as a terminal buffer
7.
8. The temperature indicates that it is a hot day
9. this is true and, and, and, and, and that is true, that is true, that is true, that is true, that is false
symbolic reduction:
```
True && (&& (&& (&& (&& True) True) True) True) False
True && (&& (&& (&& (         True) True) True) False
True && (&& (&& (                   True) True) False
True && (&& (                             True) False
True &&                                         False
False
```
10. this is true and, or, and, or, and that is true, that is false, that is true, that is false, that is true
symbolic reduction:
```
True && (|| (&& (|| (&& True) False) True) False) True
True && (|| (&& (|| (         False) True) False) True
True && (|| (&& (                    True) False) True
True && (|| (                              False) True
True &&                                           True
True
```

## 2.2.1 Exercises
1. False, and in every day langauge can indicate an ordering or implication. Mathematical conjunction is a commutative, associative operation on booleans.
2. Simplifications:
    - `0 < pi < 10`
    - `8 < p < 12`
    - `5 < x < 7`
    - `x < 4`
    - `y < 3`
    - `x = 0`

4. check that every member is true
5. check for a single member that is false
6. no, conjunction is associative
```
True & False == False & True

-- | any other combination of conjunctions can be reduced to this form i.e.
True & (True & (True & False) & (True & True))
True & (True & False          & (True & True))
True & (True & False          & True         )
True & (False                 & True         )
True & False
```
7. e, because the more conditionals you add with conjunction the more strict the requirements
8. truth table for (&)

|  a  |  b  |  a & b  |
| --- | --- | ------- |
| T   | T   | T       |
| F   | T   | F       |
| T   | F   | F       |
| F   | F   | F       |

## 2.2.2 Exercises
1. Simplifications:
    - `pi > 3`
    - `pi /= 0`
    - `pi >= 0`
    - `pi >= 0`
    - `x > 3`
3. check for one true value
4. check that all values are true
5. No, disjunction is associative
6. a, the more options the more likely there is a true value which would cause the disjunction to result in true
7. truth table for (||)

|  a  |  b  |  a || b  |
| --- | --- | -------- |
| T   | T   | T        |
| F   | T   | T        |
| T   | F   | T        |
| F   | F   | F        |

## 2.3.1 / 2.3.2 Exercises

1. truth table for (=>)

|  a  |  b  |  a => b  |
| --- | --- | -------- |
| T   | T   | T        |
| F   | T   | F        |
| T   | F   | T        |
| F   | F   | T        |

## 2.3.3 Exercises

1. truth or falsity
    - True
    - True
    - False
    - False
    - False
    - False
    - True
    - True
    - True
    - True
2. sentences in logical notation
    - T => (-D & Y) || (D & -Y)
    - T => (Y => D)
    - T => (Y & D)
    - T => (D & Y)
    - T => (-D & Y) || (D & -Y)
D = dollar is weak
Y = yuan is strong
T = trade agreement

3. truth tables

|  a  |  b  |  a => b  | -a || b |
| --- | --- | -------- | ------- |
| T   | T   | T        | T       |
| F   | T   | F        | T       |
| T   | F   | T        | F       |
| F   | F   | T        | T       |

4. Nothing of particular interest
5. more truth tables

|  a |  b  |  -b  |  a => b  |  -(a => b)  |  a & -b  |
| ---| --- | ---- | -------- | ----------- | -------- |
|  T |  T  |  F   |  T       |  F          |  F       |
|  F |  T  |  F   |  T       |  F          |  F       |
|  T |  F  |  T   |  F       |  T          |  T       |
|  F |  F  |  T   |  T       |  F          |  F       |

6. that `-(a => b)` is truth equivalent to `a & -b`

## 2.3.4 Exercises

2. truth table

|  a 	|  b 	|  -a 	|  a => b 	|  -a || b 	|
| ---	| ---	| ----	| --------	| ---------	|
|  T 	|  T 	|  F  	|  T      	|  T       	|
|  F 	|  T 	|  T  	|  T      	|  T       	|
|  T 	|  F 	|  F  	|  F      	|  F       	|
|  F 	|  F 	|  T  	|  T      	|  T       	|

3. see exercise 5 from 2.3.3

4. truth table for modus ponens

|  a 	|  b 	|  a => b 	|  a & (a =>b) 	|  (a & (a => b)) => b 	|
| ---	| ---	| --------	| -------------	| ---------------------	|
|  T 	|  T 	|  T      	|  T           	|  T                   	|
|  F 	|  T 	|  T      	|  F           	|  F                   	|
|  T 	|  F 	|  F      	|  F           	|  T                   	|
|  F 	|  F 	|  T      	|  F           	|  T                   	|

## 2.4.2 Exercises

1. Existential quantifications
    - `(∃x ∈ N)(x^3 = 27)`
    - `(∃x ∈ N)(x > 1000000)`
    - `(∃n ∈ N)(∃x ∈ N)(n / x = 0)`

2. Universal quantifications
    - `(¬∀x ∈ N)(x^3 = 28)`
    - `(∀n ∈ N)(0 < n)`
    - `(∀n ∈ N)(∀x ∈ N)(n / x = 0)`

3. Arbitray quantifications
    - `(∀e ∃s)(e <3 s)`
    - `∀e(Te || Se)`
    - `∀e∀e'(Te || Se')`
    - `¬∃s Hs`
    - `∃j ∀w (Cj => ¬Cw)`
    - `∀m ∀w (Cm => ¬Cw)`

## 2.4.4

```
(∀x ∈ N > 2)(∃y (y = 2)) Ex => Dxy
(∀x ∈ N)(∃y ∈ N)         Px => ¬Dxy
---
(∀x ∈ N > 2)             ¬(Px ∧ Exy)
```

## 2.4.5

1. logical quantifiers
    - `∀x (Sx => Px)`
    - `∃x (Fx ∧ ¬Cx)`
    - `∃x (Ex ∧ ¬Mx)`
    - `∀x (Tx => Ix)`
    - `∃x (Sx ∧ Cx) => ¬Px`
    - `∀x∃y Lxy`
    - `∃x∀y ¬Lxy`

