module RuleAlgorithm4X4
open GameBoard
open TicTacToe
open Game

let middleSquares = [6;7;10;11]
let cornerSquares = [1;4;13;16]
let sideSquares = [2;3;5;8;9;12;14;15]

let chooseMiddle4X4 (gameState : char option list) =
    if (gameState.[middleSquares.[0] - 1] = None) then
        Some(middleSquares.[0])
    elif (gameState.[middleSquares.[1] - 1] = None) then
        Some(middleSquares.[1])
    elif (gameState.[middleSquares.[2] - 1] = None) then
        Some(middleSquares.[2])
    elif (gameState.[middleSquares.[3] - 1] = None) then
        Some(middleSquares.[3])
    else
        None
      
let chooseCorner4X4 (gameState : char option list) =
    if (gameState.[cornerSquares.[0] - 1] = None) then
        Some(cornerSquares.[0])
    elif (gameState.[cornerSquares.[1] - 1] = None) then
        Some(cornerSquares.[1])
    elif (gameState.[cornerSquares.[2] - 1] = None) then
        Some(cornerSquares.[2])
    elif (gameState.[cornerSquares.[3] - 1] = None) then
        Some(cornerSquares.[3])
    else
        None

let chooseSide4X4 (gameState : char option list) = 
    if (gameState.[sideSquares.[0] - 1] = None) then
        Some(sideSquares.[0])
    elif (gameState.[sideSquares.[1] - 1] = None) then
        Some(sideSquares.[1])
    elif (gameState.[sideSquares.[2] - 1] = None) then
        Some(sideSquares.[2])
    elif (gameState.[sideSquares.[3] - 1] = None) then
        Some(sideSquares.[3])
    elif (gameState.[sideSquares.[4] - 1] = None) then
        Some(sideSquares.[4])
    elif (gameState.[sideSquares.[5] - 1] = None) then
        Some(sideSquares.[5])
    elif (gameState.[sideSquares.[6] - 1] = None) then
        Some(sideSquares.[6])
    elif (gameState.[sideSquares.[7] - 1] = None) then
        Some(sideSquares.[7])
    else
        None
let checkHorizontalWins4X4 (gameState : char option list, player : char) =
    match gameState with
    | [None;a;b;c;_;_;_;_;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(1)
    | [a;None;b;c;_;_;_;_;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(2)
    | [a;b;None;c;_;_;_;_;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(3)
    | [a;b;c;None;_;_;_;_;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(4)
    | [_;_;_;_;None;a;b;c;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(5)
    | [_;_;_;_;a;None;b;c;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(6)
    | [_;_;_;_;a;b;None;c;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(7)
    | [_;_;_;_;a;b;c;None;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(8)
    | [_;_;_;_;_;_;_;_;None;a;b;c;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(9)
    | [_;_;_;_;_;_;_;_;a;None;b;c;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(10)
    | [_;_;_;_;_;_;_;_;a;b;None;c;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(11)
    | [_;_;_;_;_;_;_;_;a;b;c;None;_;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(12)
    | [_;_;_;_;_;_;_;_;_;_;_;_;None;a;b;c] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(13)
    | [_;_;_;_;_;_;_;_;_;_;_;_;a;None;b;c] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(14)
    | [_;_;_;_;_;_;_;_;_;_;_;_;a;b;None;c] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(15)
    | [_;_;_;_;_;_;_;_;_;_;_;_;a;b;c;None] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(16)
    | _ -> None

let checkVerticalWins4X4 (gameState : char option list, player : char) =
    match gameState with
    | [None;_;_;_;a;_;_;_;b;_;_;_;c;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(1)
    | [_;None;_;_;_;a;_;_;_;b;_;_;_;c;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(2)
    | [_;_;None;_;_;_;a;_;_;_;b;_;_;_;c;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(3)
    | [_;_;_;None;_;_;_;a;_;_;_;b;_;_;_;c] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(4)
    | [a;_;_;_;None;_;_;_;b;_;_;_;c;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(5)
    | [_;a;_;_;_;None;_;_;_;b;_;_;_;c;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(6)
    | [_;_;a;_;_;_;None;_;_;_;b;_;_;_;c;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(7)
    | [_;_;_;a;_;_;_;None;_;_;_;b;_;_;_;c] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(8)
    | [a;_;_;_;b;_;_;_;None;_;_;_;c;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(9)
    | [_;a;_;_;_;b;_;_;_;None;_;_;_;c;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(10)
    | [_;_;a;_;_;_;b;_;_;_;None;_;_;_;c;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(11)
    | [_;_;_;a;_;_;_;b;_;_;_;None;_;_;_;c] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(12)
    | [a;_;_;_;b;_;_;_;c;_;_;_;None;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(13)
    | [_;a;_;_;_;b;_;_;_;c;_;_;_;None;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(14)
    | [_;_;a;_;_;_;b;_;_;_;c;_;_;_;None;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(15)
    | [_;_;_;a;_;_;_;b;_;_;_;c;_;_;_;None] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(16)
    | _ -> None

let checkDiagonalWins4X4 (gameState : char option list, player : char) =
    match gameState with
    | [None;_;_;_;_;a;_;_;_;_;b;_;_;_;_;c] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(1)
    | [a;_;_;_;_;None;_;_;_;_;b;_;_;_;_;c] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(6)
    | [a;_;_;_;_;b;_;_;_;_;None;_;_;_;_;c] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(11)
    | [a;_;_;_;_;b;_;_;_;_;c;_;_;_;_;None] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(16)
    | [_;_;_;None;_;_;a;_;_;b;_;_;c;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(4)
    | [_;_;_;a;_;_;None;_;_;b;_;_;c;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(7)
    | [_;_;_;a;_;_;b;_;_;None;_;_;c;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(10)
    | [_;_;_;a;_;_;b;_;_;c;_;_;None;_;_;_] when ((not(a = None) && a.Value = player) && (not(b = None) && b.Value = player) && (not(c = None) && c.Value = player)) -> Some(13)
    | _ -> None


let areTwoSelectedByHuman4X4 (gameState : char option list, player: char, check1, check2, check3, check4) : bool = 
    let mutable x = 0
    if(gameState.[check1 - 1].Value = player) then
        x <- x + 1
    if(gameState.[check2 - 1].Value = player) then
        x <- x + 1
    if(gameState.[check3 - 1].Value = player) then
        x <- x + 1
    if(gameState.[check4 - 1].Value = player) then
        x <- x + 1

    x > 1

let areThreeSelectedByHuman4X4 (gameState : char option list, player: char, check1, check2, check3, check4) : bool = 
    let mutable x = 0
    if(gameState.[check1 - 1].Value = player) then
        x <- x + 1
    if(gameState.[check2 - 1].Value = player) then
        x <- x + 1
    if(gameState.[check3 - 1].Value = player) then
        x <- x + 1
    if(gameState.[check4 - 1].Value = player) then
        x <- x + 1

    x > 2


let selectBlank4X4 (gameState : char option list, computerCharacter: char, check1, check2, check3, check4) =
    if(gameState.[check1 - 1] = None && not(gameState.[check2 - 1].Value = computerCharacter) && not(gameState.[check3 - 1].Value = computerCharacter) && not(gameState.[check4 - 1].Value = computerCharacter)) then
        Some(check1)
    elif(gameState.[check2 - 1] = None && not(gameState.[check1 - 1].Value = computerCharacter) && not(gameState.[check3 - 1].Value = computerCharacter) && not(gameState.[check4 - 1].Value = computerCharacter)) then
        Some(check2)
    elif(gameState.[check3 - 1] = None && not(gameState.[check1 - 1].Value = computerCharacter) && not(gameState.[check2 - 1].Value = computerCharacter) && not(gameState.[check4 - 1].Value = computerCharacter)) then
        Some(check3)
    elif(gameState.[check4 - 1] = None && not(gameState.[check1 - 1].Value = computerCharacter) && not(gameState.[check2 - 1].Value = computerCharacter) && not(gameState.[check3 - 1].Value = computerCharacter)) then
        Some(check4)
    else
        None

let blockHorizontalWin4X4 (gameState : char option list, computerCharacter : char, player : char) = 
    let mutable returnNum = None

    if(areTwoSelectedByHuman4X4(gameState,player,1,2,3,4)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,1,2,3,4)
    if(returnNum = None && areTwoSelectedByHuman4X4(gameState,player,5,6,7,8)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,5,6,7,8)
    if(returnNum = None && areTwoSelectedByHuman4X4(gameState,player,9,10,11,12)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,9,10,11,12)
    if(returnNum = None && areTwoSelectedByHuman4X4(gameState,player,13,14,15,16)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,13,14,15,16)
    returnNum

let blockVerticalWin4X4 (gameState : char option list, computerCharacter : char, player : char)  = 
    let mutable returnNum = None

    if(areTwoSelectedByHuman4X4(gameState,player,1,5,9,13)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,1,5,9,13)
    if(returnNum = None && areTwoSelectedByHuman4X4(gameState,player,2,6,10,14)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,2,6,10,14)
    if(returnNum = None && areTwoSelectedByHuman4X4(gameState,player,3,7,11,15)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,3,7,11,15)
    if(returnNum = None && areTwoSelectedByHuman4X4(gameState,player,4,8,12,16)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,4,8,12,16)
    returnNum

let blockDiagonalWin4X4 (gameState : char option list, computerCharacter : char, player : char)  = 
    let mutable returnNum = None

    if(areTwoSelectedByHuman4X4(gameState,player,1,6,11,16)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,1,6,11,16)
    if(returnNum = None && areTwoSelectedByHuman4X4(gameState,player,4,7,10,13)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,4,7,10,13)
    returnNum

let blockWin4X4 (gameState : char option list, computerCharacter : char, humanPlayer) =
    let mutable returnNum : int option = None
    returnNum <- blockHorizontalWin4X4(gameState,computerCharacter,humanPlayer)
    if(returnNum = None) then
        returnNum <- blockVerticalWin4X4(gameState,computerCharacter,humanPlayer)
        if(returnNum = None) then
            returnNum <- blockDiagonalWin4X4(gameState,computerCharacter,humanPlayer)
    returnNum

let blockThreeHorizontalWin4X4 (gameState : char option list, computerCharacter : char, player : char) = 
    let mutable returnNum = None

    if(areThreeSelectedByHuman4X4(gameState,player,1,2,3,4)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,1,2,3,4)
    if(returnNum = None && areThreeSelectedByHuman4X4(gameState,player,5,6,7,8)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,5,6,7,8)
    if(returnNum = None && areThreeSelectedByHuman4X4(gameState,player,9,10,11,12)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,9,10,11,12)
    if(returnNum = None && areThreeSelectedByHuman4X4(gameState,player,13,14,15,16)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,13,14,15,16)
    returnNum

let blockThreeVerticalWin4X4 (gameState : char option list, computerCharacter : char, player : char) = 
    let mutable returnNum = None

    if(areThreeSelectedByHuman4X4(gameState,player,1,5,9,13)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,1,5,9,13)
    if(returnNum = None && areThreeSelectedByHuman4X4(gameState,player,2,6,10,14)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,2,6,10,14)
    if(returnNum = None && areThreeSelectedByHuman4X4(gameState,player,3,7,11,15)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,3,7,11,15)
    if(returnNum = None && areThreeSelectedByHuman4X4(gameState,player,4,8,12,16)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,4,8,12,16)
    returnNum

let blockThreeDiagonalWin4X4 (gameState : char option list, computerCharacter : char, player : char) = 
    let mutable returnNum = None

    if(areThreeSelectedByHuman4X4(gameState,player,1,6,11,16)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,1,6,11,16)
    if(returnNum = None && areThreeSelectedByHuman4X4(gameState,player,4,7,10,13)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,4,7,10,13)
    returnNum

let blockThreeWin4X4 (gameState : char option list, computerCharacter : char, humanPlayer)  =
    let mutable returnNum : int option = None
    returnNum <- blockThreeHorizontalWin4X4(gameState,computerCharacter,humanPlayer)
    if(returnNum = None) then
        returnNum <- blockThreeVerticalWin4X4(gameState,computerCharacter,humanPlayer)
        if(returnNum = None) then
            returnNum <- blockThreeDiagonalWin4X4(gameState,computerCharacter,humanPlayer)
    returnNum

let winGame4X4 (gameState : char option list, computerCharacter : char) = 
    match gameState with
    | [None;a;b;c;_;_;_;_;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(1)
    | [a;None;b;c;_;_;_;_;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(2)
    | [a;b;None;c;_;_;_;_;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(3)
    | [a;b;c;None;_;_;_;_;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(4)
    | [_;_;_;_;None;a;b;c;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(5)
    | [_;_;_;_;a;None;b;c;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(6)
    | [_;_;_;_;a;b;None;c;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(7)
    | [_;_;_;_;a;b;c;None;_;_;_;_;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(8)
    | [_;_;_;_;_;_;_;_;None;a;b;c;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(9)
    | [_;_;_;_;_;_;_;_;a;None;b;c;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(10)
    | [_;_;_;_;_;_;_;_;a;b;None;c;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(11)
    | [_;_;_;_;_;_;_;_;a;b;c;None;_;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(12)
    | [_;_;_;_;_;_;_;_;_;_;_;_;None;a;b;c] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(13)
    | [_;_;_;_;_;_;_;_;_;_;_;_;a;None;b;c] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(14)
    | [_;_;_;_;_;_;_;_;_;_;_;_;a;b;None;c] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(15)
    | [_;_;_;_;_;_;_;_;_;_;_;_;a;b;c;None] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(16)
    | [None;_;_;_;a;_;_;_;b;_;_;_;c;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(1)
    | [a;_;_;_;None;_;_;_;b;_;_;_;c;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(5)
    | [a;_;_;_;b;_;_;_;None;_;_;_;c;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(9)
    | [a;_;_;_;b;_;_;_;c;_;_;_;None;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(13)
    | [_;None;_;_;_;a;_;_;_;b;_;_;_;c;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(2)
    | [_;a;_;_;_;None;_;_;_;b;_;_;_;c;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(6)
    | [_;a;_;_;_;b;_;_;_;None;_;_;_;c;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(10)
    | [_;a;_;_;_;b;_;_;_;c;_;_;_;None;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(14)
    | [_;_;None;_;_;_;a;_;_;_;b;_;_;_;c;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(3)
    | [_;_;a;_;_;_;None;_;_;_;b;_;_;_;c;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(7)
    | [_;_;a;_;_;_;b;_;_;_;None;_;_;_;c;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(11)
    | [_;_;a;_;_;_;b;_;_;_;c;_;_;_;None;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(15)
    | [_;_;_;None;_;_;_;a;_;_;_;b;_;_;_;c] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(4)
    | [_;_;_;a;_;_;_;None;_;_;_;b;_;_;_;c] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(8)
    | [_;_;_;a;_;_;_;b;_;_;_;None;_;_;_;c] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(12)
    | [_;_;_;a;_;_;_;b;_;_;_;c;_;_;_;None] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(16)
    | [None;_;_;_;_;a;_;_;_;_;b;_;_;_;_;c] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(1)
    | [a;_;_;_;_;None;_;_;_;_;b;_;_;_;_;c] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(6)
    | [a;_;_;_;_;b;_;_;_;_;None;_;_;_;_;c] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(11)
    | [a;_;_;_;_;b;_;_;_;_;c;_;_;_;_;None] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(16)
    | [_;_;_;None;_;_;a;_;_;b;_;_;c;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(4)
    | [_;_;_;a;_;_;None;_;_;b;_;_;c;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(7)
    | [_;_;_;a;_;_;b;_;_;None;_;_;c;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(10)
    | [_;_;_;a;_;_;b;_;_;c;_;_;None;_;_;_] when ((not(a = None) && a.Value = computerCharacter) && (not(b = None) && b.Value = computerCharacter) && (not(c = None) && c.Value = computerCharacter)) -> Some(13)
    | _ -> None

let rule4X4 (game : Board, humanCharacter : char, computerCharacter : char) =
    printfn "Computer move..."
    System.Threading.Thread.Sleep(1000)
    let mutable computerMove : int option = None

    computerMove <- winGame4X4(game.CurrentBoard, computerCharacter)
    if(computerMove = None) then
        computerMove <- blockThreeWin4X4(game.CurrentBoard,computerCharacter,humanCharacter)
        if(computerMove = None) then
            computerMove <- blockWin4X4(game.CurrentBoard, computerCharacter, humanCharacter)
            if(computerMove = None) then
                computerMove <- chooseMiddle4X4(game.CurrentBoard)
                if(computerMove = None) then
                    computerMove <- chooseCorner4X4(game.CurrentBoard)
                    if(computerMove = None) then
                        computerMove <- chooseSide4X4(game.CurrentBoard)
    computerMove.Value

let rule4X4Starter (board : Board, askingPlayer : Player, opposingPlayer : Player) =
    rule4X4 (board,opposingPlayer.PlayerCharacter,askingPlayer.PlayerCharacter)