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
        {IsHuman = true; ComputerAlgorithm = stupidComputerMove; PlayerCharacter = 1}
    elif answer = "2" then
        let algorithm = askAlgorithmType(print,input,gameSize)
        {IsHuman = false; ComputerAlgorithm = algorithm; PlayerCharacter = 2}
    else
        print ("Invalid input. \n")
        askForPlayerInformation (print, input, player,gameSize)

let askForGameInfo (print,input) : Board = 
    let size = getSizeOfBoard(print,input)
    let inverted = askIfInverted(print, input)
    {BoardSize = size; CurrentBoard = startingBoard(size); TurnNumber = 1; IsInverted = inverted}