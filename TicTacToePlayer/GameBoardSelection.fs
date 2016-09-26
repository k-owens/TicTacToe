module GameBoardSelection

let getSizeOfBoard (print, input : unit -> string) =
    print "Would you like to play on a 3X3 board or 4X4 board? Enter '3' for 3X3.\n"
    let boardSize = input().ToCharArray()
    if boardSize.[0] = '3' then
        3
    else
        4

let askIfInverted (print, input : unit -> string) = 
    print ("If you would like to have smallest number on top of input press the 1 key.\n")
    let answer = input().ToCharArray()
    answer.[0] = '1'