# The Real Way to Eliminate Conditionals from Your Code

- The general premise is that `if` statements and `conditionals` in general increase the complexity of code
- There are two ways to eliminate if statements
    - Move a test over an unknown value to a place where the value is known
    - Change the expression of your program's logic to not be conditional

## Eliminating Tests
- For eliminating tests, one should represent their domain with a type where only valid values are allowed. If the data is generated from an unknown source it should be validated once and then parsed to the co-responding type.
    - Conditionals of this type can be replaced with polymorphism, and boolean flags can be replaced with two functions (which are invoked correctly at the call site rather than being passed a boolean flag)

## Eliminating Conditional Logic
- Rather than using an if, consider initializing to a value

