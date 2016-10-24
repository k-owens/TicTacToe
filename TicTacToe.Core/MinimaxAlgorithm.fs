module MinimaxAlgorithm
open TicTacToe
open GameBoard
open Game

let availableBoards (gameBoard : Board, player)  =
    let moves = [for location in 0 .. (gameBoard.BoardSize*gameBoard.BoardSize - 1) -> if(gameBoard.CurrentBoard.[location] = 0) then Some(location) else None] |> List.choose id
    [for location in 0 .. (List.length moves)-1 -> {BoardSize = gameBoard.BoardSize; CurrentBoard = makeMove(gameBoard,moves.[location],player.PlayerCharacter); TurnNumber = gameBoard.TurnNumber+1; IsInverted = gameBoard.IsInverted}]



let rec minimaxAlgorithm (gameBoard : Board, isPlayer, askingPlayer : Player, opposingPlayer : Player, alpha, beta) =       
    let rec maxLoop (boardList : Board list, index, currentAlpha, currentBeta) =
        let score = minimaxAlgorithm(boardList.[index], false, askingPlayer,opposingPlayer,currentAlpha,currentBeta)
        if score >= currentBeta then
            currentBeta
        elif score > currentAlpha && index < boardList.Length-1 then
            maxLoop(boardList, index+1, score,currentBeta)
        elif score <= currentAlpha && index < boardList.Length-1 then
            maxLoop(boardList,index+1, currentAlpha, currentBeta)
        elif score > currentAlpha then
            score
        else
            currentAlpha

    let rec minLoop (boardList : Board list, index, currentAlpha, currentBeta) =
        let score = minimaxAlgorithm(boardList.[index], true, askingPlayer,opposingPlayer,currentAlpha,currentBeta)
        if score <= currentAlpha then
            currentAlpha
        elif score < currentBeta && index < boardList.Length-1 then
            minLoop(boardList, index+1, currentAlpha,score)
        elif score >= currentBeta && index < boardList.Length-1 then
            minLoop(boardList,index+1,currentAlpha,currentBeta)
        elif score < currentBeta then
            score
        else
            currentBeta
       
    if(didPlayer1Win (gameBoard.CurrentBoard, askingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
        1
    elif(didPlayer2Win (gameBoard.CurrentBoard, opposingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
        -1
    elif(didTieHappen gameBoard.CurrentBoard) then
        0
    elif isPlayer then
        let futureBoards = availableBoards(gameBoard, askingPlayer)
        let maxScore = maxLoop(futureBoards,0,alpha,beta)
        maxScore
    else
        let futureBoards = availableBoards(gameBoard, opposingPlayer)
        let minScore = minLoop(futureBoards,0,alpha,beta)
        minScore

            
let minimaxMove (gameBoard : Board, askingPlayer : Player, opposingPlayer : Player) : int =
    let moves = possibleMoves(gameBoard)
    let futureBoards = availableBoards(gameBoard, askingPlayer)
    let results = List.init (List.length futureBoards) (fun i ->minimaxAlgorithm(futureBoards.[i], false, askingPlayer,opposingPlayer, -10, 10))
    let maxScore = List.max results
    let location = List.findIndex(fun elem -> elem = maxScore) results
    moves.[location]