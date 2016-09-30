namespace TicTacToe
module GameCreator = 
    open Game
    open GameBoard
    open MinimaxAlgorithm

    let createCurrentGame (currentBoard : char list)=
        [for i in 0 .. currentBoard.Length -> if currentBoard.[i] = ' ' then None else Some(currentBoard.[i])]

    let createGame (turnNumber, currentBoard) =
        let game = {BoardSize = 3; CurrentBoard = createCurrentGame(currentBoard); TurnNumber = turnNumber; IsInverted = true}
        let player1 = {PlayerType = 1; ComputerAlgorithm = minimaxMove; PlayerCharacter = 'O'} 
        let player2 = {PlayerType = 1; ComputerAlgorithm = minimaxMove; PlayerCharacter = 'X'}
        {GameBoard = game; Players = [player1;player2]}