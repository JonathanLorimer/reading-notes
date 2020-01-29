# 7 Mistakes That Cause Fragile Code

## Summary
- Make illegal states unrepresentable
- Make structural changes when refactoring (don't just push code around)
- Take adequate time to design the boundary of your program (it is difficult to change)
- Take less time to design the internals of your program (they are relatively easy to change)
    - Consider the effect internal changes have on the rest of the program, as opposed to the particulars of the internal changes
- Don't worry about code worry about data and its relationships
- Don't be afraid to make non-local changes, this is often more economical than forging forward on unstable foundations
- Test the program's intention, not the program

## Notes
- Why checking errors is bad
    - Don't prevent errors, make them impossible
- Refactoring is not Boxing
    - Writing an essay is not about adding punctuation, its about thinking about what you want to say and saying it differently
    - Refactoring is not moving code into functions and modules
    - When refactoring one should change the structure and design
- Why some decisions can't be undone
    - The boundaries of your program need to be designed exceptionally well because you're going to be living with them for a long time
- Don't overthink the interior
    - Time can be spared on the interior of the codebase so long as the boundaries are well designed and maintained
    - Consider the effect code changes have on the rest of the codebase
- Code quality is not about code
    - Bad programmers worry about the code. Good programmers worry about the data and their relationships
- Getting stuck in an old design
    - It is easy to get stuck thinking that you cannot change surrounding code
    - Don't be afraid to make non-local changes
- Are you missing half of what makes good tests?
    - Test the program's intention, not the program



