# Hagfish

Authors: Yuhao Liu (liuyuhao), Gefei Zhu (kenzgf)

Hagfish is a small chess engine in Haskell that supports playing with it in command line. app/Main.hs acts as an entry point to the engine, and boots it up. In src, there are 3 pairs of file, with one acting as the abstraction of the other. Starting with Game.hs and Chess.hs, the former describes any general two-player game, and the latter defines the specific game of chess, containing all the rules of chess as well as some formatter. Similarly, Strategy.hs describes an interface of an evaluation of the position and the bestmove forward, each with a default implementation. ChessStrategy.hs contains the specific evaluation and algorithms we used to produce the best move. Lastly, Engine.hs contains an abstracted interactive IO engine, while ChessEngine.hs implements the details of the engine. Console.hs is a file with console related helper functions that are used in ChessEngine.hs. In the test folder, Spec.hs contains a set of property tests.

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

