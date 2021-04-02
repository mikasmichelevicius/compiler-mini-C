# compiler-mini-C

Theoretical and practical aspects covered of building and designing a compiler. Using LLVM and C++ a recursive descent parser for a subset of the C programming language is developed. 

-- Part 1.
Parser is created from a language specification, the AST is produced from the code parsed. (Includes building and running the lexer, creating a recursive descent parser, grammar transformation to LL(k) equivalent available for top-down parsing, building an AST and displaying it after a successful parsing)
-- Part 2.
Code is generated from the AST created by using LLVM IR Code generation. Binary executable is built from IR generated from the compiler.
