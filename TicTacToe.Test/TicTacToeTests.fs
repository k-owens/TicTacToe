module TicTacToeTests

open Xunit
open TicTacToe
open Game
open GameBoard 
open MoveManager 
open MinimaxAlgorithm
open RuleAlgorithm3X3
open RuleAlgorithm4X4
open StupidComputer
open GameCreator

let printHolder (arbitrary : string) =
    printfn ""

let stringOutput () =
    "1"

[<Fact>]
let canTie () =
    let board = {BoardSize = 3; CurrentBoard = [1;2;1;1;2;1;2;1;2]; TurnNumber = 1; IsInverted = true}
    let player1 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 1}
    let player2 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 2}
    let game = {GameBoard = board; Players = [player1; player2]}
    Assert.True(isGameOver (game))

[<Fact>]
let willNotEndGameTooSoon () =
    let board = {BoardSize = 3; CurrentBoard = [1;0;1;1;2;1;2;0;2]; TurnNumber = 1; IsInverted = true}
    let player1 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 1}
    let player2 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 2}
    let game = {GameBoard = board; Players = [player1; player2]}
    Assert.True(not(isGameOver (game)))

[<Fact>]
let willHorizontalWinEndGame () =    
    let board1 = {BoardSize = 3; CurrentBoard = [1;1;1;0;0;0;0;0;0]; TurnNumber = 1; IsInverted = true}
    let board2 = {BoardSize = 3; CurrentBoard = [0;0;0;1;1;1;0;0;0]; TurnNumber = 1; IsInverted = true}
    let board3 = {BoardSize = 3; CurrentBoard = [0;0;0;0;0;0;1;1;1]; TurnNumber = 1; IsInverted = true}
    let player1 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 1}
    let player2 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 2}
    let game1 = {GameBoard = board1; Players = [player1; player2]}
    let game2 = {GameBoard = board2; Players = [player1; player2]}
    let game3 = {GameBoard = board3; Players = [player1; player2]}

    Assert.True(isGameOver (game1))
    Assert.True(isGameOver (game2))
    Assert.True(isGameOver (game3))


[<Fact>]
let willVerticalWinEndGame () =
    let board1 = {BoardSize = 3; CurrentBoard = [1;0;0;1;0;0;1;0;0]; TurnNumber = 1; IsInverted = true}
    let board2 = {BoardSize = 3; CurrentBoard = [0;1;0;0;1;0;0;1;0]; TurnNumber = 1; IsInverted = true}
    let board3 = {BoardSize = 3; CurrentBoard = [0;0;1;0;0;1;0;0;1]; TurnNumber = 1; IsInverted = true}
    let player1 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 1}
    let player2 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 2}
    let game1 = {GameBoard = board1; Players = [player1; player2]}
    let game2 = {GameBoard = board2; Players = [player1; player2]}
    let game3 = {GameBoard = board3; Players = [player1; player2]}

    Assert.True(isGameOver (game1))
    Assert.True(isGameOver (game2))
    Assert.True(isGameOver (game3))


[<Fact>]
let willDiagonalWinEndGame () =
    let board1 = {BoardSize = 3; CurrentBoard = [1;0;0;0;1;0;0;0;1]; TurnNumber = 1; IsInverted = true}
    let board2 = {BoardSize = 3; CurrentBoard = [0;0;1;0;1;0;1;0;0]; TurnNumber = 1; IsInverted = true}
    let player1 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 1}
    let player2 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 2}
    let game1 = {GameBoard = board1; Players = [player1; player2]}
    let game2 = {GameBoard = board2; Players = [player1; player2]}
    Assert.True(isGameOver (game1))
    Assert.True(isGameOver (game2))

[<Fact>]
let canMakeMove () =
    let game = {BoardSize = 3; CurrentBoard = [0;0;0;0;0;0;0;0;0]; TurnNumber = 1; IsInverted = true}
    
    Assert.Equal<int list>([1;0;0;0;0;0;0;0;0], makeMove (game, 0, 1))

[<Fact>]
let minimaxTest () =    
    let board = {BoardSize = 3; CurrentBoard = [2;0;0;0;0;0;0;0;0]; TurnNumber = 1; IsInverted = false}
    let player1 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 1}
    let player2 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 2}
    Assert.Equal<int>(4,minimaxMove(board, player1, player2))

[<Fact>]
let canComputerChooseCorner () =
    let testBoard : int list = [0;0;0;0;0;0;0;0]
    Assert.Equal<int>(chooseCorner(testBoard), 3)

[<Fact>]
let canComputerChooseSide () =
    let testBoard : int list = [0;0;0;0;0;0;0;0]
    Assert.Equal<int>(2, chooseSide(testBoard))

[<Fact>]
let canComputerChooseCornerInBetweenHumanMove () =
    let testBoard : int list = [0;1;0;1;0;0;0;0;0]
    Assert.Equal<int>(1, chooseCornerInBetween(4,testBoard,2))

[<Fact>]
let doesComputerKnowWhenFirstTurn () =
    let testBoard : int list = [0;0;0;1;0;0;0;0;0]
    Assert.True(isFirstComputerTurn(testBoard,1))

[<Fact>]
let doesComputerKnowWhenNotFirstTurn () =
    let testBoard : int list = [0;1;0;1;0;0;0;0;0]
    Assert.True(not(isFirstComputerTurn(testBoard,1)))

[<Fact>]
let doesComputerRespondToFirstMoveMiddleCorrectly () = 
    let testBoard : int list = [0;0;0;0;1;0;0;0;0]
    Assert.Equal<int>(1, respondToFirstMoveMiddle(testBoard,5,1))

[<Fact>]
let doesComputerRespondToFirstMoveCornerCorrectly () = 
    let testBoard : int list = [1;0;0;0;0;0;0;0;0]
    Assert.Equal<int>(5, respondToFirstMoveCorner(testBoard,1,1))

[<Fact>]
let doesComputerRespondToFirstMoveSideCorrectly () = 
    let testBoard : int list = [0;1;0;0;0;0;0;0;0]
    Assert.Equal<int>(5, respondToFirstMoveSide(testBoard,1))

[<Fact>]
let doesComputerRespondToCorrectFirstMove () =
    let testBoard : int list = [1;0;0;0;0;0;0;0;0]
    Assert.Equal<int>(5, respondToFirstMove(testBoard,1,1))

[<Fact>]
let canComputerChooseCornerSquare4X4 () =
    let testBoard = [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]

    Assert.Equal<int>(1,chooseCorner4X4(testBoard).Value)

[<Fact>]
let canComputerChooseSideSquare4X4 () =
    let testBoard = [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]

    Assert.Equal<int>(2,chooseSide4X4(testBoard).Value)

[<Fact>]
let canComputerFindHorizontalWins4X4 () =
    let testBoard1 = [0;1;1;1;0;0;0;0;0;0;0;0;0;0;0;0]
    let testBoard2 = [1;0;1;1;0;0;0;0;0;0;0;0;0;0;0;0]
    let testBoard3 = [1;1;0;1;0;0;0;0;0;0;0;0;0;0;0;0]
    let testBoard4 = [1;1;1;0;0;0;0;0;0;0;0;0;0;0;0;0]
    let testBoard5 = [0;0;0;0;0;1;1;1;0;0;0;0;0;0;0;0]
    let testBoard6 = [0;0;0;0;1;0;1;1;0;0;0;0;0;0;0;0]
    let testBoard7 = [0;0;0;0;1;1;0;1;0;0;0;0;0;0;0;0]
    let testBoard8 = [0;0;0;0;1;1;1;0;0;0;0;0;0;0;0;0]
    let testBoard9 = [0;0;0;0;0;0;0;0;0;1;1;1;0;0;0;0]
    let testBoard10 = [0;0;0;0;0;0;0;0;1;0;1;1;0;0;0;0]
    let testBoard11 = [0;0;0;0;0;0;0;0;1;1;0;1;0;0;0;0]
    let testBoard12 = [0;0;0;0;0;0;0;0;1;1;1;0;0;0;0;0]
    let testBoard13 = [0;0;0;0;0;0;0;0;0;0;0;0;0;1;1;1]
    let testBoard14 = [0;0;0;0;0;0;0;0;0;0;0;0;1;0;1;1]
    let testBoard15 = [0;0;0;0;0;0;0;0;0;0;0;0;1;1;0;1]
    let testBoard16 = [0;0;0;0;0;0;0;0;0;0;0;0;1;1;1;0]
    let testBoard17 = [0;0;0;0;0;0;0;0;0;0;0;0;1;1;0;0]

    Assert.Equal<int>(1,checkHorizontalWins4X4(testBoard1,1).Value)
    Assert.Equal<int>(2,checkHorizontalWins4X4(testBoard2,1).Value)
    Assert.Equal<int>(3,checkHorizontalWins4X4(testBoard3,1).Value)
    Assert.Equal<int>(4,checkHorizontalWins4X4(testBoard4,1).Value)
    Assert.Equal<int>(5,checkHorizontalWins4X4(testBoard5,1).Value)
    Assert.Equal<int>(6,checkHorizontalWins4X4(testBoard6,1).Value)
    Assert.Equal<int>(7,checkHorizontalWins4X4(testBoard7,1).Value)
    Assert.Equal<int>(8,checkHorizontalWins4X4(testBoard8,1).Value)
    Assert.Equal<int>(9,checkHorizontalWins4X4(testBoard9,1).Value)
    Assert.Equal<int>(10,checkHorizontalWins4X4(testBoard10,1).Value)
    Assert.Equal<int>(11,checkHorizontalWins4X4(testBoard11,1).Value)
    Assert.Equal<int>(12,checkHorizontalWins4X4(testBoard12,1).Value)
    Assert.Equal<int>(13,checkHorizontalWins4X4(testBoard13,1).Value)
    Assert.Equal<int>(14,checkHorizontalWins4X4(testBoard14,1).Value)
    Assert.Equal<int>(15,checkHorizontalWins4X4(testBoard15,1).Value)
    Assert.Equal<int>(16,checkHorizontalWins4X4(testBoard16,1).Value)
    //Assert.Equal<int option>(0,checkHorizontalWins4X4(testBoard17,1))

[<Fact>]
let canComputerFindVerticalWins4X4 () =
    let testBoard1 = [0;0;0;0;1;0;0;0;1;0;0;0;1;0;0;0]
    let testBoard2 = [0;0;0;0;0;1;0;0;0;1;0;0;0;1;0;0]
    let testBoard3 = [0;0;0;0;0;0;1;0;0;0;1;0;0;0;1;0]
    let testBoard4 = [0;0;0;0;0;0;0;1;0;0;0;1;0;0;0;1]
    let testBoard5 = [1;0;0;0;0;0;0;0;1;0;0;0;1;0;0;0]
    let testBoard6 = [0;1;0;0;0;0;0;0;0;1;0;0;0;1;0;0]
    let testBoard7 = [0;0;1;0;0;0;0;0;0;0;1;0;0;0;1;0]
    let testBoard8 = [0;0;0;1;0;0;0;0;0;0;0;1;0;0;0;1]
    let testBoard9 = [1;0;0;0;1;0;0;0;0;0;0;0;1;0;0;0]
    let testBoard10 = [0;1;0;0;0;1;0;0;0;0;0;0;0;1;0;0]
    let testBoard11 = [0;0;1;0;0;0;1;0;0;0;0;0;0;0;1;0]
    let testBoard12 = [0;0;0;1;0;0;0;1;0;0;0;0;0;0;0;1]
    let testBoard13 = [1;0;0;0;1;0;0;0;1;0;0;0;0;0;0;0]
    let testBoard14 = [0;1;0;0;0;1;0;0;0;1;0;0;0;0;0;0]
    let testBoard15 = [0;0;1;0;0;0;1;0;0;0;1;0;0;0;0;0]
    let testBoard16 = [0;0;0;1;0;0;0;1;0;0;0;1;0;0;0;0]
    let testBoard17 = [0;0;0;1;0;0;0;1;0;0;0;0;0;0;0;0]

    Assert.Equal<int>(1,checkVerticalWins4X4(testBoard1, 1).Value)
    Assert.Equal<int>(2,checkVerticalWins4X4(testBoard2, 1).Value)
    Assert.Equal<int>(3,checkVerticalWins4X4(testBoard3, 1).Value)
    Assert.Equal<int>(4,checkVerticalWins4X4(testBoard4, 1).Value)
    Assert.Equal<int>(5,checkVerticalWins4X4(testBoard5, 1).Value)
    Assert.Equal<int>(6,checkVerticalWins4X4(testBoard6, 1).Value)
    Assert.Equal<int>(7,checkVerticalWins4X4(testBoard7, 1).Value)
    Assert.Equal<int>(8,checkVerticalWins4X4(testBoard8, 1).Value)
    Assert.Equal<int>(9,checkVerticalWins4X4(testBoard9, 1).Value)
    Assert.Equal<int>(10,checkVerticalWins4X4(testBoard10, 1).Value)
    Assert.Equal<int>(11,checkVerticalWins4X4(testBoard11, 1).Value)
    Assert.Equal<int>(12,checkVerticalWins4X4(testBoard12, 1).Value)
    Assert.Equal<int>(13,checkVerticalWins4X4(testBoard13, 1).Value)
    Assert.Equal<int>(14,checkVerticalWins4X4(testBoard14, 1).Value)
    Assert.Equal<int>(15,checkVerticalWins4X4(testBoard15, 1).Value)
    Assert.Equal<int>(16,checkVerticalWins4X4(testBoard16, 1).Value)
    //Assert.Equal<int option>(0,checkVerticalWins4X4(testBoard17, 1))

[<Fact>]
let canComputerFindDiagonalWins () =
    let testBoard1 = [0;0;0;0;0;1;0;0;0;0;1;0;0;0;0;1]
    let testBoard2 = [1;0;0;0;0;0;0;0;0;0;1;0;0;0;0;1]
    let testBoard3 = [1;0;0;0;0;1;0;0;0;0;0;0;0;0;0;1]
    let testBoard4 = [1;0;0;0;0;1;0;0;0;0;1;0;0;0;0;0]
    let testBoard5 = [0;0;0;0;0;0;1;0;0;1;0;0;1;0;0;0]
    let testBoard6 = [0;0;0;1;0;0;0;0;0;1;0;0;1;0;0;0]
    let testBoard7 = [0;0;0;1;0;0;1;0;0;0;0;0;1;0;0;0]
    let testBoard8 = [0;0;0;1;0;0;1;0;0;1;0;0;0;0;0;0]

    Assert.Equal<int>(1,checkDiagonalWins4X4(testBoard1,1).Value)
    Assert.Equal<int>(6,checkDiagonalWins4X4(testBoard2,1).Value)
    Assert.Equal<int>(11,checkDiagonalWins4X4(testBoard3,1).Value)
    Assert.Equal<int>(16,checkDiagonalWins4X4(testBoard4,1).Value)
    Assert.Equal<int>(4,checkDiagonalWins4X4(testBoard5,1).Value)
    Assert.Equal<int>(7,checkDiagonalWins4X4(testBoard6,1).Value)
    Assert.Equal<int>(10,checkDiagonalWins4X4(testBoard7,1).Value)
    Assert.Equal<int>(13,checkDiagonalWins4X4(testBoard8,1).Value)

[<Fact>]
let canStupidComputerMakeMove () =    
    let board = {BoardSize = 3; CurrentBoard = [1;1;0;0;2;0;0;0;0]; TurnNumber = 1; IsInverted = false}
    let player1 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 1}
    let player2 = {IsHuman = true; ComputerAlgorithm = minimaxMove; PlayerCharacter = 2}
    let testMove = stupidComputerMove(board,player1,player2) 
    Assert.True(testMove > 1 && testMove < 9 && not(testMove = 4))

[<Fact>]
let canGamesBeCreated () =
    let game = createGame(4,[1;1;0;0;2;0;0;0;0],true,true);
    Assert.Equal<int>(4,game.GameBoard.TurnNumber);
    Assert.Equal<int list>([1;1;0;0;2;0;0;0;0],game.GameBoard.CurrentBoard);
    Assert.True(game.Players.Head.IsHuman);
    Assert.True(game.Players.Tail.Head.IsHuman);