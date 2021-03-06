﻿namespace TicTacToe 
module GamePlayer =
    open GameBoard
    open MoveManager
    open MinimaxAlgorithm
    open Game
    open StupidComputer


    let returnFunction () =
        0


    let playerTurn (game : Game, playerNumber, moveNumber : int) =
            makeMove(game.GameBoard, moveTypeSelector (game, playerNumber,moveNumber), game.Players.[playerNumber-1].PlayerCharacter)


    let turn (game : Game, moveNumber) =
        if(game.GameBoard.TurnNumber % 2 = 1 ) then
            playerTurn(game,1, moveNumber)
        else
            playerTurn(game,2, moveNumber)


    let playGame (game : Game, moveNumber) =
        if not(isGameOver (game)) then
            let newBoardState = turn(game, moveNumber)
            let updatedBoard = {BoardSize = game.GameBoard.BoardSize; CurrentBoard = newBoardState; TurnNumber = game.GameBoard.TurnNumber+1; IsInverted = game.GameBoard.IsInverted}
            let newGame = {GameBoard = updatedBoard; Players = game.Players}
            newGame
        else
            game


    let blankGame =
        let game = {BoardSize = 3; CurrentBoard = startingBoard(3); TurnNumber = 1; IsInverted = true}
        let player1 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 1} //playerType and playerCharacter need to be variables
        let player2 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 2}
        {GameBoard = game; Players = [player1;player2]}