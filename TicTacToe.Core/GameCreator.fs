namespace TicTacToe
module GameCreator = 
    open Game
    open GameBoard
    open MinimaxAlgorithm


    let createGame (turnNumber, currentBoard, player1Type, player2Type, player1Character, player2Character) =
        let game = {BoardSize = 3; CurrentBoard = currentBoard; TurnNumber = turnNumber; IsInverted = true}
        let player1 = {IsHuman = player1Type; ComputerAlgorithm = minimaxMove; PlayerCharacter = player1Character} 
        let player2 = {IsHuman = player2Type; ComputerAlgorithm = minimaxMove; PlayerCharacter = player2Character}
        {GameBoard = game; Players = [player1;player2]}