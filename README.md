# functions-forever

Our Term Project is a command-line interpreter for a small custom language, the interactive interpreters for Python or Haskell. We have decided on this idea because Haskell is well suited for building compilers, such as algebraic data types for defining recursive ASTs and pattern matching for easily inspecting them. This project also draws heavily from the class itself, since we will be defining a context-free grammar and writing our tokenizer and parser based on it. Again, Haskell makes it easy for the code to mirror a grammar in Backus-Naur form. To make the language easy to understand and use, it has syntax similar to C and have semantics similar to Python and Javascript.

The interpreter is currently in development. It will be able to: evaluate arithmetic expressions, declare and use variables persistent throughout a session, change execution flow according to code structure (e.g. loops, if-branching), define and call functions with their own code and scope, and display different classes of errors with helpful messages (such as invalid syntax or undefined variables). The frontend consists of the terminal REPL with users entering text, formatting and printing evaluation results, and displaying errors. The backend is the actual language pipeline: tokenizing raw text, parsing those tokens, verifying and resolving the code’s structure, and execution with persistent state in an environment.

## Usage

This project uses the Glasgow Haskell Compiler. To install it, go to https://www.haskell.org/ghcup/ and install GHCup, following the instructions on the site (if not already installed). 

### Running the starter programs

Open a terminal and navigate to the directory where the file is saved (in this case, `starter-programs`).

Compile the code using this command (replacing `program.hs` with the Haskell source file you would like to compile):
```sh
ghc Program.hs
```
If it has run without error, run the compiled program by running (again, replacing `Program` with the name of the file, without the `.hs` extension):
```sh
./Program
```
If you are in a Windows environment, you can run the file like so:
```sh
.\Program.exe
```
Note: successful compilation will leave extra artifact files (`program.hi` and `program.o`) in the working directory.

### Running the Term Project

The Term Project is compiled slightly differently. It still uses `ghc`, but with some extra flags. In the base directory (the same level as this README), run this command:
```sh
ghc -isrc -outputdir build -o main src/Main.hs
```
Again, if it has run without error, it will create an executable in the current directory called `main` (or `main.exe` on Windows).