module ConsoleTasks
open TicTacToe
open Game

let rec moveInput (moveEntered, print) = 
    print ("Please select your move: ")
    let input = moveEntered()
    try
        input |> System.Int32.Parse
    with
        | :? System.FormatException -> moveInput(moveEntered, print)


let displayInvertedBoard (board : Board) =
    let mutable x = ""
    for i in 0 .. board.BoardSize-1 do
        x <-  x + "___________\n"
        x <- x + "|"
        for j in 0 .. board.BoardSize-1 do
            if board.CurrentBoard.[(i*board.BoardSize) + j] = 0 then
                x <- x + ' '.ToString()
            else
                x <- x + board.CurrentBoard.[(i*board.BoardSize) + j].ToString()
            x <- x + "|"
        x <- x + "\n"
    x <- x + "___________\n"
    x


let displayUninvertedBoard (board : Board) =
    let mutable x = ""
    for i in board.BoardSize-1 .. -1 .. 0 do
        x <-  x + "___________\n"
        x <- x + "|"
        for j in board.BoardSize-1 .. -1 .. 0 do
            if(board.CurrentBoard.[(i*board.BoardSize) + j] = 0) then
                x <- x + ' '.ToString()
            else
                x <- x + board.CurrentBoard.[(i*board.BoardSize) + j].ToString()
            x <- x + "|"
        x <- x + "\n"
    x <- x + "___________\n"
    x

let displayBoard (game : Game) =
    if(game.GameBoard.IsInverted) then
        "Current board:\n" + displayInvertedBoard(game.GameBoard)
    else
        "Current board:\n" + displayUninvertedBoard(game.GameBoard)


let displayInvertedBoardOptions (boardSize) =
    let mutable x = ""
    for i in 0 .. boardSize-1 do
        x <- x + "___________\n"
        x <- x + "|"
        for j in 0 .. boardSize-1 do
            x <- x + ((i*boardSize) + j).ToString()
            x <- x + "|"
        x <- x + "\n"
    x <- x + "___________\n"
    x

let displayUnivertedBoardOptions (boardSize) =
    let mutable x = ""
    for i in boardSize-1 .. -1 .. 0 do
        x <- x + "___________\n"
        printf "|"
        for j in boardSize-1 .. -1 .. 0 do
            x <- x + ((i*boardSize) + j).ToString()
            x <- x + "|"
        x <- x + "\n"
    x <- x + "___________\n"
    x

let displayBoardOptions (game : Game) = 
    if(game.GameBoard.IsInverted) then
        "Board input:\n" + displayInvertedBoardOptions(game.GameBoard.BoardSize)
    else
        "Board input:\n" + displayUnivertedBoardOptions (game.GameBoard.BoardSize)


let displayPlayerTurn(game : Game) =
    if game.GameBoard.TurnNumber % 2 = 1 then
        "Player 1's turn:"
    else
        "Player 2's turn:"