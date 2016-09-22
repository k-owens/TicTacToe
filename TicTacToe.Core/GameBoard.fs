module GameBoard
open TicTacToe
open Game

let startingBoard (size) : char option list = 
    [for i in 0 .. (size*size) -> None]


let isLegalMove (move, game) = 
    move >= 0 && move < game.BoardSize*game.BoardSize && game.CurrentBoard.[move] = None


let makeMove (game, moveIndex, player) =
    List.init (game.BoardSize * game.BoardSize) (fun i -> if moveIndex = i then Some(player) else game.CurrentBoard.[i])


let didTieHappen (gameState) =
    not(List.exists (fun elem -> elem = None) gameState)


let didWin (row: char option list, player1) =
    List.forall (fun elem -> not(elem = None) && elem.Value = player1) row


let didHorizontalWinHappen (gameState : char option list, player1, boardSize) =
    let rows = List.init boardSize (fun elem -> List.init boardSize (fun i -> gameState.[i+(elem * boardSize)]))
    List.exists (fun elem -> didWin (elem, player1)) rows


let didVerticalWinHappen (gameState : char option list, player1, boardSize) =
    let columns = List.init boardSize (fun elem -> List.init boardSize (fun i -> gameState.[(i * boardSize)+elem]))
    List.exists (fun elem -> didWin (elem, player1)) columns


let didDiagonalWinHappen (gameState : char option list, player1, boardSize) =
    let diagonal1 = List.init boardSize (fun i -> gameState.[i+(boardSize*i)])
    let diagonal2 = List.init boardSize (fun i -> gameState.[(i*(boardSize-1)) + (boardSize - 1)])
    didWin (diagonal1, player1) || didWin (diagonal2, player1)


let didPlayer1Win (gameState, player1, boardSize) =
    didHorizontalWinHappen (gameState, player1, boardSize)
    || didVerticalWinHappen (gameState, player1, boardSize)
    || didDiagonalWinHappen (gameState, player1, boardSize)


let didPlayer2Win (gameState, player2, boardSize) =
    didHorizontalWinHappen (gameState, player2, boardSize)
    || didVerticalWinHappen (gameState, player2, boardSize)
    || didDiagonalWinHappen (gameState, player2, boardSize)


let didSomeoneWin (gameState, player1, player2, boardSize) = 
    didPlayer1Win (gameState, player1, boardSize)
    || didPlayer2Win (gameState, player2, boardSize)


let isGameOver (game : Game) = 
    didTieHappen (game.GameBoard.CurrentBoard) || didSomeoneWin (game.GameBoard.CurrentBoard, game.Players.[0].PlayerCharacter, game.Players.[1].PlayerCharacter, game.GameBoard.BoardSize)