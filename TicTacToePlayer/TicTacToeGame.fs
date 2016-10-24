module TicTacToeGame
open TicTacToe
open GamePlayer
open UserSelection
open Game
open ConsoleTasks
open GameBoard


let enterInput () =
    System.Console.ReadLine()


let keepWindowOpen () =
    printfn "Enter any key to exit."
    System.Console.ReadKey() |> ignore
    0


let printToScreen (output) =
    printfn "%s" output


let isLegalMove (move, game) = 
    move >= 0 && move < game.BoardSize*game.BoardSize && game.CurrentBoard.[move] = 0


let rec runGame (print: (string -> unit), input : (unit -> string), game) =
    printToScreen(displayBoard(game))
    printToScreen(displayPlayerTurn(game))
    printToScreen(displayBoardOptions(game))
    if not(isGameOver (game)) then
        let move = moveInput(enterInput,printToScreen)
        if not(isLegalMove(move,game.GameBoard)) then
            printToScreen("Illegal move.")
            runGame(print,input,game)
        else
            let updatedGame = playGame(game, move)
            runGame(print, input, updatedGame)
    else if askIfReplay (print, input) then
        runGame(print, input, blankGame)
    else
        0


[<EntryPoint>]
let main (args : string[]) =
    let newGame = blankGame
    let over = runGame(printToScreen, enterInput, newGame)
    keepWindowOpen()