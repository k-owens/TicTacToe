namespace TicTacToe
module GameCreator = 
    open Game
    open GameBoard
    open MinimaxAlgorithm

    let createCurrentGame (currentBoard : char list)=
        [for i in 0 .. currentBoard.Length-1 -> if currentBoard.[i] = '_' then None else Some(currentBoard.[i])]

    let createGame (turnNumber, currentBoard, player1Type, player2Type) =
        let game = {BoardSize = 3; CurrentBoard = createCurrentGame(currentBoard); TurnNumber = turnNumber; IsInverted = true}
        let player1 = {IsHuman = player1Type; ComputerAlgorithm = minimaxMove; PlayerCharacter = 'O'} 
        let player2 = {IsHuman = player2Type; ComputerAlgorithm = minimaxMove; PlayerCharacter = 'X'}
        {GameBoard = game; Players = [player1;player2]}