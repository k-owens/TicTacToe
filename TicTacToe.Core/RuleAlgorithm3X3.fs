module RuleAlgorithm3X3

open Game
open GameBoard

let middleSquare = 5
let outsideSquares = [ 2; 4; 6; 8 ]
let cornerSquares = [ 1; 3; 7; 9 ]

let checkHorizontalWins (userCharacter: char, gameState : char option list) = 
    let mutable returnNum = None
    for i in 0 .. 3 .. 8 do
        if(gameState.[i] = None && gameState.[i+1].Value = userCharacter && gameState.[i+2].Value = userCharacter) then
            returnNum <- Some(i)
        elif(gameState.[i].Value = userCharacter && gameState.[i+1] = None && gameState.[i+2].Value = userCharacter) then
            returnNum <- Some(i + 1)
        elif(gameState.[i].Value = userCharacter && gameState.[i+1].Value = userCharacter && gameState.[i+2] = None) then
            returnNum <- Some(i + 2)
    returnNum

let checkVerticalWins (userCharacter: char, gameState : char option list) =
    let mutable returnNum = None
    for j in 0 .. 2 do
        if(gameState.[j] = None && gameState.[j+3].Value = userCharacter && gameState.[j+6].Value = userCharacter) then
            returnNum <- Some(j)
        elif(gameState.[j].Value = userCharacter && gameState.[j+3] = None && gameState.[j+6].Value = userCharacter) then
            returnNum <- Some(j + 3)
        elif(gameState.[j].Value = userCharacter && gameState.[j+3].Value = userCharacter && gameState.[j+6] = None) then
            returnNum <- Some(j + 6)
    returnNum

let checkDiagonalWins (userCharacter: char, gameState : char option list) =
    let mutable returnNum = None
    if(gameState.[0] = None && gameState.[4].Value = userCharacter && gameState.[8].Value = userCharacter) then
        returnNum <- Some(0)
    elif(gameState.[0].Value = userCharacter && gameState.[4] = None && gameState.[8].Value = userCharacter) then
        returnNum <- Some(4)
    elif(gameState.[0].Value = userCharacter && gameState.[4].Value = userCharacter && gameState.[8] = None) then
        returnNum <- Some(8)

    if(gameState.[2] = None && gameState.[4].Value = userCharacter && gameState.[6].Value = userCharacter) then
        returnNum <- Some(2)
    elif(gameState.[2].Value = userCharacter && gameState.[4] = None && gameState.[6].Value = userCharacter) then
        returnNum <- Some(4)
    elif(gameState.[2].Value = userCharacter && gameState.[4].Value = userCharacter && gameState.[6] = None) then
        returnNum <- Some(6)
    returnNum

let winGameOrBlockWin (userCharacter: char, gameState : char option list) = 
    let mutable returnNum = None

    returnNum <- checkHorizontalWins (userCharacter, gameState)
    if(returnNum = None) then
        returnNum <- checkVerticalWins (userCharacter, gameState)
        if(returnNum = None) then
            returnNum <- checkDiagonalWins (userCharacter, gameState)
    Some(returnNum.Value + 1)

let chooseCorner (gameState : char option list)  = 
    if(gameState.[cornerSquares.[1] - 1] = None) then
        Some(cornerSquares.[1])
    elif(gameState.[cornerSquares.[2] - 1] = None) then
        Some(cornerSquares.[2])
    elif(gameState.[cornerSquares.[3] - 1] = None) then
        Some(cornerSquares.[3])
    elif(gameState.[cornerSquares.[0] - 1] = None) then
        Some(cornerSquares.[0])
    else
        None

let chooseSide (gameState : char option list) = 
    if(gameState.[outsideSquares.[0] - 1] = None) then
        Some(outsideSquares.[0])
    elif(gameState.[outsideSquares.[1] - 1] = None) then
        Some(outsideSquares.[1])
    elif(gameState.[outsideSquares.[2] - 1] = None) then
        Some(outsideSquares.[2])
    elif(gameState.[outsideSquares.[3]] = None) then
        Some(outsideSquares.[3])
    else
        None

let chooseCornerInBetween (huMove : int, gameState : char option list, firstHumanMove : int) =
    if((firstHumanMove = cornerSquares.[0] && huMove = cornerSquares.[3]) || (firstHumanMove = cornerSquares.[3] && huMove = cornerSquares.[0]) || (firstHumanMove = cornerSquares.[1] && huMove = cornerSquares.[2]) || (firstHumanMove = cornerSquares.[2] && huMove = cornerSquares.[1])) then
        chooseSide (gameState)
    elif((firstHumanMove = outsideSquares.[0] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[2])) || (firstHumanMove = cornerSquares.[1] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[2])) || (firstHumanMove = outsideSquares.[1] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[1])) || (firstHumanMove = cornerSquares.[2] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[1]))) then
        Some(cornerSquares.[0])
    elif((firstHumanMove = outsideSquares.[0] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[3])) || (firstHumanMove = cornerSquares.[0] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[3])) || (firstHumanMove = outsideSquares.[2] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[0])) || (firstHumanMove = cornerSquares.[3] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[0]))) then
        Some(cornerSquares.[1])
    elif((firstHumanMove = outsideSquares.[1] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[3])) || (firstHumanMove = cornerSquares.[0] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[3])) || (firstHumanMove = outsideSquares.[3] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[0])) || (firstHumanMove = cornerSquares.[3] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[0]))) then
        Some(cornerSquares.[2])
    elif((firstHumanMove = outsideSquares.[2] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[2])) || (firstHumanMove = cornerSquares.[1] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[2])) || (firstHumanMove = outsideSquares.[3] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[1])) || (firstHumanMove = cornerSquares.[2] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[1]))) then
        Some(cornerSquares.[3])
    else
        None

let isFirstComputerTurn (gameState : char option list, humanCharacter : char) = 
    match gameState with
    | [humanCharacter; None;None;None;None;None;None;None;None] -> true
    | [None;humanCharacter;None;None;None;None;None;None;None] -> true
    | [None;None;humanCharacter;None;None;None;None;None;None] -> true
    | [None;None;None;humanCharacter;None;None;None;None;None] -> true
    | [None;None;None;None;humanCharacter;None;None;None;None] -> true
    | [None;None;None;None;None;humanCharacter;None;None;None] -> true
    | [None;None;None;None;None;None;humanCharacter;None;None] -> true
    | [None;None;None;None;None;None;None;humanCharacter;None] -> true
    | [None;None;None;None;None;None;None;None;humanCharacter] -> true
    | _ -> false

let respondToFirstMoveMiddle (gameState : char option list, humanMoveSpot : int, humanCharacter : char)  = 
    if(humanMoveSpot = middleSquare && isFirstComputerTurn(gameState, humanCharacter)) then
        Some(cornerSquares.[0])
    else 
        None

let respondToFirstMoveCorner (gameState : char option list, humanMoveSpot : int, humanCharacter : char) = 
    if((humanMoveSpot = cornerSquares.[0] || humanMoveSpot = cornerSquares.[1] || humanMoveSpot = cornerSquares.[2] || humanMoveSpot = cornerSquares.[3]) && isFirstComputerTurn(gameState, humanCharacter)) then
        Some(middleSquare)
    else
        None

let respondToFirstMoveSide (gameState : char option list, humanCharacter : char) = 
    if(isFirstComputerTurn(gameState,humanCharacter)) then
        Some(middleSquare)
    else
        None

let isFirstMove (gamestate : char option list) : bool =
    gamestate = [None; None; None; None; None; None; None; None; None]


let makeFirstMove (gamestate) =
    if(isFirstMove(gamestate)) then
        Some(1)
    else
        None

let respondToFirstMove (gameState : char option list, humanMoveSpot : int, humanCharacter : char) =
    let mutable returnMove = None

    if(returnMove = None) then
        returnMove <- respondToFirstMoveMiddle(gameState, humanMoveSpot, humanCharacter)
        if(returnMove = None) then
            returnMove <- respondToFirstMoveCorner(gameState, humanMoveSpot, humanCharacter)
            if(returnMove = None) then
                returnMove <- respondToFirstMoveSide(gameState, humanCharacter)
    returnMove


let respondToMiddleStrategy (gameState : char option list, firstHumanMove : int, humanCharacter : char, computerCharacter : char) = 
    let mutable returnMove = None
    if(firstHumanMove = middleSquare) then
        returnMove <- winGameOrBlockWin (computerCharacter, gameState)
        if(returnMove = None) then
            returnMove <- winGameOrBlockWin (humanCharacter, gameState)
        if(returnMove = None) then
            returnMove <- chooseCorner (gameState)
            if(returnMove = None) then
                returnMove <- chooseSide (gameState)
    returnMove

let respondToSideOrCornerStrategy (gameState : char option list, humanMoveSpot : int, firstHumanMove : int, humanCharacter : char, computerCharacter : char) = 
    let mutable returnMove = None
    returnMove <- winGameOrBlockWin (computerCharacter, gameState)
    if(returnMove = None) then
        returnMove <- winGameOrBlockWin (humanCharacter, gameState)
        if(returnMove = None) then
            returnMove <- chooseCornerInBetween (humanMoveSpot, gameState, firstHumanMove)
            if(returnMove = None) then
                returnMove <- chooseCorner (gameState)
                if(returnMove = None) then
                    returnMove <- chooseSide (gameState)
    returnMove

let rule3X3 (game : Board, humanMoveSpot : int, firstHumanMove : int, humanCharacter : char, computerCharacter : char)=
    printfn "Computer move..."
    let mutable computerMove = None

    computerMove <- makeFirstMove(game.CurrentBoard)
    if(computerMove = None) then
        computerMove <- respondToFirstMove (game.CurrentBoard, humanMoveSpot, humanCharacter)
        if(computerMove = None) then
            computerMove <- respondToMiddleStrategy (game.CurrentBoard, firstHumanMove, humanCharacter, computerCharacter)
            if(computerMove = None) then
                computerMove <- respondToSideOrCornerStrategy (game.CurrentBoard, humanMoveSpot, firstHumanMove, humanCharacter, computerCharacter)
    computerMove.Value

let rule3X3Starter (game : Board, askingPlayer : Player, opposingPlayer : Player) =
    rule3X3(game,0,0,opposingPlayer.PlayerCharacter,askingPlayer.PlayerCharacter)
