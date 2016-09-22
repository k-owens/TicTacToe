module MoveManager
open TicTacToe
open Game
open GameBoard
open MinimaxAlgorithm
open RuleAlgorithm3X3
open RuleAlgorithm4X4
open StupidComputer



let rec humanMove (spaceInput,game : Game) =
    let move = spaceInput
    if isLegalMove (move, game.GameBoard) then
        move
    else
        humanMove (spaceInput,game)


let computerMove (game : Game, playerNumber) = 
    game.Players.[playerNumber-1].ComputerAlgorithm (game.GameBoard, game.Players.[playerNumber-1], otherPlayer(game,playerNumber))

let moveTypeSelector (game : Game, playerNumber, spaceInput) = 
    if(game.Players.[playerNumber-1].PlayerType = 1) then
        humanMove (spaceInput,game)
    else
        computerMove (game, playerNumber)