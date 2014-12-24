HaskellMinesweeper
==================

##Haskell Project 2014
  - Minesweeper game implementation
  - Graphical User Interface
  - Quickcheck
  - Extremely Basic Solver


## Game Implementation
This project implements a copy of the 'popular' game Minesweeper in Haskell. It is fully playable through the GUI (interface.hs).

The project uses wxHaskell for the GUI, but most of the other libraries are standard with the Haskell Platform. 

There is one third party function, an implementation matrix reductions to reduced row echelon form. This has been isolated into the file 'Thirdparty.hs'.

## Usage
Setup cabal sandbox as follows:

```sh
$ git clone https://github.com/ghodgins/HaskellMinesweeper.git
$ cd HaskellMinesweeper
$ cabal sandbox init 
$ cabal install --only-dependencies
```

After preparing the sandbox it is possible to run the minesweeper program via the command below.

`$ cabal run`

Alternatively in order to test the program

`$ cabal test`

## Authors
 - Kevin Bluett (GUI & Solver)
 - Geoffrey Hodgins (Gamelogic)
 - Ciaran Finn (Gamelogic & quickcheck)

## Solver
The solver implemented is quite basic. What has currently been implemented is a system of linear equations based on the visible frontier within the game. The visible frontier are inserted into a matrix based, which is then processed down to reduced row-echelon form (Due to severe time constraints the reduction is performed by a third-party function which can be seen in 'Thirdparty.hs', GNU code from the rosetta foundation). Using some basic heuristics, the solver makes some basic moves, ie. opening and flagging squares - but this is only done when it is logically sound to do so.

It was planned to increase the number of heuristics and introduce a probabistic analysis of the board in order for the solver to be fully indpendent from the player of the game.

As it stands when the solver cannot make a basic moves unless it has logicaly precedent. This means it cannot play the opening moves and is quickly blocked from making further mores after opening up.

## Todo/Issues
- Refactor the game implementation (make more monadic and readable, generally use best-practices)
- Make code more resilient to error (also ensuring by increasing the coverage by Quickcheck)
- Add effects to the GUI (transitions etc)
- Increase the efficiency of the program, perhaps making it parallel (particularly the solver)
- Complete the solver to calculate probabilities and iron out remaining bugs in the solver.

## Testing
A number of QuickCheck tests are implemented, and can be viewed in the quickCheck.hs file. They test mainly the game logic.

In order to test the program please setup the sandbox as per 'usage' section and then use the following command.

`$ cabal test`

## Screenshots
![](/screenshots/splash.jpg?raw=true "Splash Screen" =250px)
![](/screenshots/game.jpg?raw=true "Game" =250px)
![](/screenshots/game1.jpeg?raw=true "Game 1" =250px)
![](/screenshots/lose.jpg?raw=true "Lose" =250px)
![](/screenshots/soclose.jpeg?raw=true "So Close" =250px)
![](/screenshots/win.jpg?raw=true "So Close" =250px)
