# functions-forever

## Usage

This project uses the Glasgow Haskell Compiler. To install it, go to https://www.haskell.org/ghcup/ and install GHCup, following the instructions on the site (if not already installed). 

To begin, open a terminal and navigate to the directory where the file is saved.

Compile the code using this command (replacing `program.hs` with the Haskell source file you would like to compile):
```
ghc program.hs
```
If it has run without error, run the compiled program by running (again, replacing `program` with the name of the file, without the `.hs` extension):
```
./program
```
If you are in a Windows environment, you can run the file like so:
```
.\program.exe
```
Note: successful compilation will leave extra artifact files (`program.hi` and `program.o`) in the working directory.
