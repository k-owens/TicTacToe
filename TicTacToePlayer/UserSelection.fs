module UserSelection
open TicTacToe
open Game
open GameBoard
open PlayerSelection
open GameBoardSelection
open StupidComputer

let askIfReplay (print, input : unit -> string) = 
    print ("If you would like to play again please press the Y key.\n")
    let answer = input().ToCharArray()
    answer.[0] = 'y' || answer.[0] = 'Y'

let rec askForPlayerInformation (print, input : unit -> string, player : int, gameSize) =
    print ("Please select what type of player Player " + player.ToString() + " is: \n")
    print ("1. Human\n")
    print ("2. Computer\n")
    let answer = input()
    let symbol = getPlayerCharacter(print, input)
    if answer = "1" then
        let seed = answer |> System.Int32.Parse
        {PlayerType = seed; ComputerAlgorithm = stupidComputerMove; PlayerCharacter = symbol}
    elif answer = "2" then
        let seed = answer |> System.Int32.Parse
        let algorithm = askAlgorithmType(print,input,gameSize)
        {PlayerType = seed; ComputerAlgorithm = algorithm; PlayerCharacter = symbol}
    else
        print ("Invalid input. \n")
        askForPlayerInformation (print, input, player,gameSize)

let askForGameInfo (print,input) : Board = 
    let size = getSizeOfBoard(print,input)
    let inverted = askIfInverted(print, input)
    {BoardSize = size; CurrentBoard = startingBoard(size); TurnNumber = 1; IsInverted = inverted}