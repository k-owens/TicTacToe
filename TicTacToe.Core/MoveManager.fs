module MoveManager
open TicTacToe
open Game
open GameBoard
open MinimaxAlgorithm
open RuleAlgorithm3X3
open RuleAlgorithm4X4
open StupidComputer


let computerMove (game : Game, playerNumber) = 
    game.Players.[playerNumber-1].ComputerAlgorithm (game.GameBoard, game.Players.[playerNumber-1], otherPlayer(game,playerNumber))

let moveTypeSelector (game : Game, playerNumber, spaceInput) = 
    if(game.Players.[playerNumber-1].PlayerType = 1) then
        spaceInput
    else
        computerMove (game, playerNumber)