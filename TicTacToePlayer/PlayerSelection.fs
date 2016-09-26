module PlayerSelection
open MinimaxAlgorithm
open RuleAlgorithm3X3
open RuleAlgorithm4X4
open StupidComputer

let getPlayerCharacter (print, input : unit -> string) =
    print "Please enter the character that will symbolize the moves for this player:\n"
    let userCharacter = input().ToCharArray()
    print "\n"
    userCharacter.[0]

let verifyCharacters (userCharacter, compCharacter) =
    not(userCharacter = ' ' || compCharacter = ' ' || userCharacter = '\r' || compCharacter = '\r' || userCharacter = compCharacter|| userCharacter = '_' || compCharacter = '_' )

let rec askAlgorithmType (print, input : unit -> string, gameSize) = 
    print ("Please select an algorithm to play against:\n")
    print ("1. minimax\n")
    print ("2. stupid\n")
    if(gameSize = 3 || gameSize = 4) then
        print ("3. rules\n")
    let answer = input()
    if(answer = "1" )then
        minimaxMove
    elif(answer = "2") then
        stupidComputerMove
    elif(answer = "3" && gameSize = 3) then
        rule3X3Starter
    elif(answer = "3" && gameSize = 4) then
        rule4X4Starter
    else
        print ("Invalid input. \n")
        askAlgorithmType (print, input, gameSize)