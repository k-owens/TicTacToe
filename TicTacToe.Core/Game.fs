namespace TicTacToe
module Game =

    type Board = 
        {   BoardSize : int 
            CurrentBoard : char option list 
            TurnNumber : int
            IsInverted : bool}

    type Player =
        {   PlayerType : int
            ComputerAlgorithm : (Board * Player * Player -> int)
            PlayerCharacter: char}

    type Game =
        {   GameBoard : Board
            Players : Player list}

    let otherPlayer(game : Game, playerNumber) =
        if(playerNumber = 1) then
            game.Players.[1]
        else
            game.Players.[0]

    let possibleMoves (gameBoard) =
        [for location in 0 .. (gameBoard.BoardSize*gameBoard.BoardSize - 1) -> if(gameBoard.CurrentBoard.[location] = None) then Some(location) else None] |> List.choose id
