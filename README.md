HaskellMinesweeper
==================

##Haskell Project 2014
  - Minesweeper game implementation
  - Graphical User Interface
  - Quickcheck
  - AI Solver


## Game Implementation
This project implements a copy of the well known game Minesweeper in Haskell. It is playable through the GUI (interface.hs), most features can also be demonstrated through the CLI version (Main.hs).

The project uses wxHaskell for the GUI, but most of the other libraries are standard with the Haskell Platform.


## AI Solver
Blah blah blah

## Usage
```sh
$ git clone https://github.com/ghodgins/HaskellMinesweeper.git
$ cd HaskellMinesweeper/src
$ ghc interface.hs
$ ./interface
```

##Authors
 - Kevin Bluett
 - Geoffrey Hodgins
 - Ciaran Finn

##Todo/Issues
- Refactor the game implementation (make more monadic and readable, generally use best-practices)
- Make code more resilient to error (also ensuring by increasing the usage of Quickcheck)
- Add effects to the GUI (transitions etc)
- Increase the efficiency of the program, perhaps making it parallel (particularly the AI solver)

## Testing
A number of QuickCheck tests are implemented. While several tests run fine and test what we want to be tested, the rest unfortunately do not. Due to time constraint, not all of the custom data types are correctly generating arbitrary test data and therefore testing of the more complicated functions is not possible.


## Screenshots
![](/screenshots/splash.jpg?raw=true "Splash Screen" =250px)
![](/screenshots/game.jpg?raw=true "Game" =250px)
![](/screenshots/game1.jpeg?raw=true "Game 1" =250px)
![](/screenshots/lose.jpg?raw=true "Lose" =250px)
![](/screenshots/soclose.jpeg?raw=true "So Close" =250px)
![](/screenshots/win.jpg?raw=true "So Close" =250px)