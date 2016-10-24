module RuleAlgorithm3X3

open TicTacToe
open Game
open GameBoard

let middleSquare = 5
let outsideSquares = [ 2; 4; 6; 8 ]
let cornerSquares = [ 1; 3; 7; 9 ]

let checkHorizontalWins (userCharacter: int, gameState : int list) = 
    let mutable returnNum = 0
    for i in 0 .. 3 .. 8 do
        if(gameState.[i] = 0 && gameState.[i+1] = userCharacter && gameState.[i+2] = userCharacter) then
            returnNum <- i
        elif(gameState.[i] = userCharacter && gameState.[i+1] = 0 && gameState.[i+2] = userCharacter) then
            returnNum <- i + 1
        elif(gameState.[i] = userCharacter && gameState.[i+1] = userCharacter && gameState.[i+2] = 0) then
            returnNum <- i + 2
    returnNum

let checkVerticalWins (userCharacter: int, gameState : int list) =
    let mutable returnNum = 0
    for j in 0 .. 2 do
        if(gameState.[j] = 0 && gameState.[j+3] = userCharacter && gameState.[j+6] = userCharacter) then
            returnNum <- j
        elif(gameState.[j] = userCharacter && gameState.[j+3] = 0 && gameState.[j+6] = userCharacter) then
            returnNum <- j + 3
        elif(gameState.[j] = userCharacter && gameState.[j+3] = userCharacter && gameState.[j+6] = 0) then
            returnNum <- j + 6
    returnNum

let checkDiagonalWins (userCharacter: int, gameState : int list) =
    let mutable returnNum = 0
    if(gameState.[0] = 0 && gameState.[4] = userCharacter && gameState.[8] = userCharacter) then
        returnNum <- 0
    elif(gameState.[0] = userCharacter && gameState.[4] = 0 && gameState.[8] = userCharacter) then
        returnNum <- 4
    elif(gameState.[0] = userCharacter && gameState.[4] = userCharacter && gameState.[8] = 0) then
        returnNum <- 8

    if(gameState.[2] = 0 && gameState.[4] = userCharacter && gameState.[6] = userCharacter) then
        returnNum <- 2
    elif(gameState.[2] = userCharacter && gameState.[4] = 0 && gameState.[6] = userCharacter) then
        returnNum <- 4
    elif(gameState.[2] = userCharacter && gameState.[4] = userCharacter && gameState.[6] = 0) then
        returnNum <- 6
    returnNum

let winGameOrBlockWin (userCharacter: int, gameState : int list) = 
    let mutable returnNum = 0

    returnNum <- checkHorizontalWins (userCharacter, gameState)
    if(returnNum = 0) then
        returnNum <- checkVerticalWins (userCharacter, gameState)
        if(returnNum = 0) then
            returnNum <- checkDiagonalWins (userCharacter, gameState)
    returnNum + 1

let chooseCorner (gameState : int list)  = 
    if(gameState.[cornerSquares.[1] - 1] = 0) then
        cornerSquares.[1]
    elif(gameState.[cornerSquares.[2] - 1] = 0) then
        cornerSquares.[2]
    elif(gameState.[cornerSquares.[3] - 1] = 0) then
        cornerSquares.[3]
    elif(gameState.[cornerSquares.[0] - 1] = 0) then
        cornerSquares.[0]
    else
        0

let chooseSide (gameState : int list) = 
    if(gameState.[outsideSquares.[0] - 1] = 0) then
        outsideSquares.[0]
    elif(gameState.[outsideSquares.[1] - 1] = 0) then
        outsideSquares.[1]
    elif(gameState.[outsideSquares.[2] - 1] = 0) then
        outsideSquares.[2]
    elif(gameState.[outsideSquares.[3]] = 0) then
        outsideSquares.[3]
    else
        0

let chooseCornerInBetween (huMove : int, gameState : int list, firstHumanMove : int) =
    if((firstHumanMove = cornerSquares.[0] && huMove = cornerSquares.[3]) || (firstHumanMove = cornerSquares.[3] && huMove = cornerSquares.[0]) || (firstHumanMove = cornerSquares.[1] && huMove = cornerSquares.[2]) || (firstHumanMove = cornerSquares.[2] && huMove = cornerSquares.[1])) then
        chooseSide (gameState)
    elif((firstHumanMove = outsideSquares.[0] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[2])) || (firstHumanMove = cornerSquares.[1] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[2])) || (firstHumanMove = outsideSquares.[1] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[1])) || (firstHumanMove = cornerSquares.[2] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[1]))) then
        cornerSquares.[0]
    elif((firstHumanMove = outsideSquares.[0] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[3])) || (firstHumanMove = cornerSquares.[0] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[3])) || (firstHumanMove = outsideSquares.[2] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[0])) || (firstHumanMove = cornerSquares.[3] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[0]))) then
        cornerSquares.[1]
    elif((firstHumanMove = outsideSquares.[1] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[3])) || (firstHumanMove = cornerSquares.[0] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[3])) || (firstHumanMove = outsideSquares.[3] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[0])) || (firstHumanMove = cornerSquares.[3] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[0]))) then
        cornerSquares.[2]
    elif((firstHumanMove = outsideSquares.[2] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[2])) || (firstHumanMove = cornerSquares.[1] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[2])) || (firstHumanMove = outsideSquares.[3] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[1])) || (firstHumanMove = cornerSquares.[2] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[1]))) then
        cornerSquares.[3]
    else
        0

let isFirstComputerTurn (gameState : int list, humanCharacter : int) = 
    match gameState with
    | [humanCharacter; 0;0;0;0;0;0;0;0] -> true
    | [0;humanCharacter;0;0;0;0;0;0;0] -> true
    | [0;0;humanCharacter;0;0;0;0;0;0] -> true
    | [0;0;0;humanCharacter;0;0;0;0;0] -> true
    | [0;0;0;0;humanCharacter;0;0;0;0] -> true
    | [0;0;0;0;0;humanCharacter;0;0;0] -> true
    | [0;0;0;0;0;0;humanCharacter;0;0] -> true
    | [0;0;0;0;0;0;0;humanCharacter;0] -> true
    | [0;0;0;0;0;0;0;0;humanCharacter] -> true
    | _ -> false

let respondToFirstMoveMiddle (gameState : int list, humanMoveSpot : int, humanCharacter : int)  = 
    if(humanMoveSpot = middleSquare && isFirstComputerTurn(gameState, humanCharacter)) then
        cornerSquares.[0]
    else 
        0

let respondToFirstMoveCorner (gameState : int list, humanMoveSpot : int, humanCharacter : int) = 
    if((humanMoveSpot = cornerSquares.[0] || humanMoveSpot = cornerSquares.[1] || humanMoveSpot = cornerSquares.[2] || humanMoveSpot = cornerSquares.[3]) && isFirstComputerTurn(gameState, humanCharacter)) then
        middleSquare
    else
        0

let respondToFirstMoveSide (gameState : int list, humanCharacter : int) = 
    if(isFirstComputerTurn(gameState,humanCharacter)) then
        middleSquare
    else
        0

let isFirstMove (gamestate : int list) : bool =
    gamestate = [0; 0; 0; 0; 0; 0; 0; 0; 0]


let makeFirstMove (gamestate) =
    if(isFirstMove(gamestate)) then
        1
    else
        0

let respondToFirstMove (gameState : int list, humanMoveSpot : int, humanCharacter : int) =
    let mutable returnMove = 0

    if(returnMove = 0) then
        returnMove <- respondToFirstMoveMiddle(gameState, humanMoveSpot, humanCharacter)
        if(returnMove = 0) then
            returnMove <- respondToFirstMoveCorner(gameState, humanMoveSpot, humanCharacter)
            if(returnMove = 0) then
                returnMove <- respondToFirstMoveSide(gameState, humanCharacter)
    returnMove


let respondToMiddleStrategy (gameState : int list, firstHumanMove : int, humanCharacter : int, computerCharacter : int) = 
    let mutable returnMove = 0
    if(firstHumanMove = middleSquare) then
        returnMove <- winGameOrBlockWin (computerCharacter, gameState)
        if(returnMove = 0) then
            returnMove <- winGameOrBlockWin (humanCharacter, gameState)
        if(returnMove = 0) then
            returnMove <- chooseCorner (gameState)
            if(returnMove = 0) then
                returnMove <- chooseSide (gameState)
    returnMove

let respondToSideOrCornerStrategy (gameState : int list, humanMoveSpot : int, firstHumanMove : int, humanCharacter : int, computerCharacter : int) = 
    let mutable returnMove = 0
    returnMove <- winGameOrBlockWin (computerCharacter, gameState)
    if(returnMove = 0) then
        returnMove <- winGameOrBlockWin (humanCharacter, gameState)
        if(returnMove = 0) then
            returnMove <- chooseCornerInBetween (humanMoveSpot, gameState, firstHumanMove)
            if(returnMove = 0) then
                returnMove <- chooseCorner (gameState)
                if(returnMove = 0) then
                    returnMove <- chooseSide (gameState)
    returnMove

let rule3X3 (game : Board, humanMoveSpot : int, firstHumanMove : int, humanCharacter : int, computerCharacter : int)=
    printfn "Computer move..."
    let mutable computerMove = 0

    computerMove <- makeFirstMove(game.CurrentBoard)
    if(computerMove = 0) then
        computerMove <- respondToFirstMove (game.CurrentBoard, humanMoveSpot, humanCharacter)
        if(computerMove = 0) then
            computerMove <- respondToMiddleStrategy (game.CurrentBoard, firstHumanMove, humanCharacter, computerCharacter)
            if(computerMove = 0) then
                computerMove <- respondToSideOrCornerStrategy (game.CurrentBoard, humanMoveSpot, firstHumanMove, humanCharacter, computerCharacter)
    computerMove

let rule3X3Starter (game : Board, askingPlayer : Player, opposingPlayer : Player) =
    rule3X3(game,0,0,opposingPlayer.PlayerCharacter,askingPlayer.PlayerCharacter)
