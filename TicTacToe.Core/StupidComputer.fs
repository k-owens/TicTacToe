module StupidComputer
open GameBoard
open Game

let randomGenerator = System.Random()

let rec stupidComputerMove (gameBoard, player1 : Player, player2 : Player) = 
    let moveChoices = possibleMoves(gameBoard)

    let randomMove = randomGenerator.Next(0,moveChoices.Length-1)
    moveChoices.[randomMove]