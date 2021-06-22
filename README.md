# Modern-Compiler-Implementation-in-ML-book

Techniques, data structures, and algorithms for translating programming
languages into executable code.

A compiler comprises many phases, each operating on a different abstract
language.

## Tiger

Tiger is a simple but nontrivial language of the Algol family.

* Nested scope
* Heap-allocated records

## Phases

We implement each phase as one or more software modules.

Any software system is much easier to understand and implement if the
designer takes care of the fundamental abstractions and interfaces.

1. Source program
2. Lex
3. Tokens
4. Parse
5. Reductions
6. Parsing actions
7. Abstract Syntax
8. Semantic Analysis
    1. Tables
    2. Environments
9. Translate
10. Translate
11. IR Trees
12. Canonicalize
13. IR Trees
14. Instruction Selection
15. Assem
16. Control Flow Analysis
17. Flow Graph
18. Data Flow Analysis
19. Interference Graph
20. Register Allocation
21. Register Assignment
22. Code Emission
23. Assembly Language
24. Assembler
25. Relocatable Object Code
26. Linker
27. Machine Language

## Implementation Language

The interfaces between modules of the compiler are almost as important as the
algorithms inside the modules.

Here we use ML to describe the interfaces concretely.
