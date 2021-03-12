// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type PieceTypes =
    | Rook
    | Knight
    | Bishop
    | Queen
    | King
    | Pawn

type Side =
    | White
    | Black

type Piece(pieceColor:Side, typeOfPiece: PieceTypes, coord: (int * int)) =
    member _.Color = pieceColor
    member _.PieceType = typeOfPiece
    member _.Coord = coord

type Board(pieces:List<Piece>, turn :Side) =
    member _.Pieces = pieces
    member _.Turn = turn


// End of type declarations

let StartingFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
let FriedLiverFEN = "r1bqk2r/pppp1ppp/2n2n2/2b1p1N1/2B1P3/8/PPPP1PPP/RNBQK2R w KQkq - 6 5"
let TestingFEN = "rnb1kbnr/ppp1pppp/8/4q3/8/2N5/PPPPKPPP/R1BQ1BNR w KQkq - 2 4"

let CoordinateToSquare (coord:(int * int)) =
    let move = (
        match fst coord with
        | 1 -> "a"
        | 2 -> "b"
        | 3 -> "c"
        | 4 -> "d"
        | 5 -> "e"
        | 6 -> "f"
        | 7 -> "g"
        | 8 -> "h"
        +
        (snd coord).ToString()

    )
    move
// This function would convert (1,1) to a1 or (5,4) to e4

let SquareToCoordinate (input:String) =
    let number = (
        match (Seq.toList input).[0] with
        | 'a' -> 1
        | 'b' -> 2
        | 'c' -> 3
        | 'd' -> 4
        | 'e' -> 5
        | 'f' -> 6
        | 'g' -> 7
        | 'h' -> 8
        ,
        match (Seq.toList input).[1] with
        | '1' -> 1
        | '2' -> 2
        | '3' -> 3
        | '4' -> 4
        | '5' -> 5
        | '6' -> 6
        | '7' -> 7
        | '8' -> 8
    )
    number

let MatchPiece x (coord:(int * int)) =
    match x with
    | "R" -> Some(White, Rook, coord)
    | "N" -> Some(White, Knight, coord)
    | "B" -> Some(White, Bishop, coord)
    | "Q" -> Some(White, Queen, coord)
    | "K" -> Some(White, King, coord)
    | "P" -> Some(White, Pawn, coord)
    | "r" -> Some(Black, Rook, coord)
    | "n" -> Some(Black, Knight, coord)
    | "b" -> Some(Black, Bishop, coord)
    | "q" -> Some(Black, Queen, coord)
    | "k" -> Some(Black, King, coord)
    | "p" -> Some(Black, Pawn, coord)
    | _ -> None

let ComputeFEN (boardState:Board) = 
    let mutable testString = ""
    let mutable runningTotal = 0
    for y = 8 downto 1 do
        for x in 1 .. 8 do
            let mutable SpaceFilled = false
            for p in 0 .. boardState.Pieces.Length - 1 do
                if boardState.Pieces.[p].Coord = (x, y)
                then
                    SpaceFilled <- true

                    if runningTotal > 0 
                    then 
                        testString <- testString + runningTotal.ToString()
                        runningTotal <- 0

                    match boardState.Pieces.[p].Color with
                    | White -> 
                        match boardState.Pieces.[p].PieceType with
                        | Rook -> testString <- testString + "R"
                        | Knight -> testString <- testString + "N"
                        | Bishop -> testString <- testString + "B"
                        | Queen -> testString <- testString + "Q"
                        | King -> testString <- testString + "K"
                        | Pawn -> testString <- testString + "P" 
                    | Black ->
                        match boardState.Pieces.[p].PieceType with
                        | Rook -> testString <- testString + "r"
                        | Knight -> testString <- testString + "n"
                        | Bishop -> testString <- testString + "b"
                        | Queen -> testString <- testString + "q"
                        | King -> testString <- testString + "k"
                        | Pawn -> testString <- testString + "p"
            
            if SpaceFilled = false
            then runningTotal <- runningTotal + 1

        if runningTotal > 0
        then 
            testString <- testString + runningTotal.ToString()
            runningTotal <- 0

        if y > 1
        then testString <- testString + "/"   
    testString <- testString + " w KQkq - 0 1"
    testString

let ReadFEN (x:string) =
    let strings = x.Split('/', ' ')
    let AllPieces:List<Piece> = [
    let mutable ycount = 1
    for i = 7 downto 0 do
        let c = strings.[i]
        let mutable xcount = 0
        for x in 0 .. c.Length - 1 do
            let CharIsNumber = c.[x] |> Char.IsDigit
            if CharIsNumber
                then
                    let num = c.[x].ToString() |> Int32.Parse
                    xcount <- num + xcount
                else
                    //let test = MatchPiece (c.[x].ToString()) (xcount + 1, ycount) |> Option.map Piece
                    let test = MatchPiece (c.[x].ToString()) (xcount + 1, ycount) |> Option.map Piece
                    match test with
                    | Some(x) -> x
                    | None -> ()
                    xcount <- xcount + 1
            
        ycount <- ycount + 1
    ]
    AllPieces

let CalculateMaterialBalance (boardState:Board) = 
    let materialBalance = [
    for p in 0 .. boardState.Pieces.Length - 1 do
        match boardState.Pieces.[p].Color with
        | White ->
            match boardState.Pieces.[p].PieceType with
            | Rook -> 5
            | Knight -> 3
            | Bishop -> 3
            | Queen -> 9
            | King -> 0
            | Pawn -> 1
            
        | Black ->
            match boardState.Pieces.[p].PieceType with
            | Rook -> -5
            | Knight -> -3
            | Bishop -> -3
            | Queen -> -9
            | King -> 0
            | Pawn -> -1
    ]
    materialBalance |> List.sum

let mutable GameBoard = Board((ReadFEN TestingFEN), White)

let CheckForHorizontalMoves (range:int) (piece:Piece) = 
    let OpenSquares : List<(int * int)> = [
    for it in 0 .. 3 do
        let mutable xsearch = fst piece.Coord
        let mutable ysearch = snd piece.Coord
        match it with
        | 0 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch + 1
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then i <- range + 99
                        else
                            if ysearch < 9 
                            then
                                i <- i + 1
                                (xsearch, ysearch)
                            else
                                i <- range + 99
        | 1 -> let mutable i = 1
               while (i < range) do
                    xsearch <- xsearch + 1
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then i <- range + 99
                        else
                            if xsearch < 9 
                            then
                                i <- i + 1
                                (xsearch, ysearch);
                            else
                                i <- range + 99
        | 2 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch - 1
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then i <- range + 99
                        else
                            if ysearch > 0 
                            then
                                i <- i + 1
                                (xsearch, ysearch)
                            else
                                i <- range + 99
        | 3 -> let mutable i = 1
               while (i < range) do
                   xsearch <- xsearch - 1
                   if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then i <- range + 99
                        else
                            if xsearch > 0 
                            then
                                i <- i + 1
                                (xsearch, ysearch)
                            else
                                i <- range + 99
    ]
    OpenSquares

let CheckForHorizontalCaptures (range:int) (piece:Piece) = 
    let Captures : List<(Piece)> = [
    for it in 0 .. 3 do
        let mutable xsearch = fst piece.Coord
        let mutable ysearch = snd piece.Coord
        match it with
        | 0 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch + 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                        then
                            for q in 0 .. GameBoard.Pieces.Length - 1 do
                                if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                                    then 
                                        if piece.Color <> GameBoard.Pieces.[q].Color
                                            then GameBoard.Pieces.[q]
                                        i <- range + 99
                    i <- i + 1
        | 1 -> let mutable i = 1
               while (i < range) do
                    xsearch <- xsearch + 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. GameBoard.Pieces.Length - 1 do
                            if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> GameBoard.Pieces.[q].Color
                                        then GameBoard.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
        | 2 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch - 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. GameBoard.Pieces.Length - 1 do
                            if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> GameBoard.Pieces.[q].Color
                                        then GameBoard.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
        | 3 -> let mutable i = 1
               while (i < range) do
                    xsearch <- xsearch - 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. GameBoard.Pieces.Length - 1 do
                            if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> GameBoard.Pieces.[q].Color
                                        then GameBoard.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
    ]
    Captures

let CheckForDiagonalMoves (range:int) (piece:Piece) = 
    let OpenSquares : List<(int * int)> = [
    for it in 0 .. 3 do
        let mutable xsearch = fst piece.Coord
        let mutable ysearch = snd piece.Coord
        match it with
        | 0 ->  let mutable i = 1
                while (i < range) do
                    ysearch <- ysearch + 1
                    xsearch <- xsearch + 1
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then i <- range + 99
                        else
                            if ysearch < 9 && xsearch < 9 
                            then
                                i <- i + 1
                                (xsearch, ysearch)
                            else
                                i <- range + 99
        | 1 ->  let mutable i = 1
                while (i < range) do
                    ysearch <- ysearch - 1
                    xsearch <- xsearch + 1
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then i <- range + 99
                        else
                            if ysearch > 0 && xsearch < 9 
                            then
                                i <- i + 1
                                (xsearch, ysearch)
                            else
                                i <- range + 99
        | 2 ->  let mutable i = 1
                while (i < range) do
                    ysearch <- ysearch - 1
                    xsearch <- xsearch - 1
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then i <- range + 99
                        else
                            if ysearch > 0 && xsearch > 0
                            then
                                i <- i + 1
                                (xsearch, ysearch)
                            else
                                i <- range + 99
        | 3 ->  let mutable i = 1
                while (i < range) do
                    ysearch <- ysearch + 1
                    xsearch <- xsearch - 1
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then i <- range + 99
                        else
                            if ysearch < 9 && xsearch > 0 
                            then
                                i <- i + 1
                                (xsearch, ysearch)
                            else
                                i <- range + 99
    ]
    OpenSquares

let CheckForDiagonalCaptures (range:int) (piece:Piece) = 
    let Captures : List<(Piece)> = [
    for it in 0 .. 3 do
        let mutable xsearch = fst piece.Coord
        let mutable ysearch = snd piece.Coord
        match it with
        | 0 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch + 1
                    xsearch <- xsearch + 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                        then
                            for q in 0 .. GameBoard.Pieces.Length - 1 do
                                if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                                    then 
                                        if piece.Color <> GameBoard.Pieces.[q].Color
                                            then GameBoard.Pieces.[q]
                                        i <- range + 99
                    i <- i + 1
        | 1 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch - 1
                    xsearch <- xsearch + 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. GameBoard.Pieces.Length - 1 do
                            if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> GameBoard.Pieces.[q].Color
                                        then GameBoard.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
        | 2 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch - 1
                    xsearch <- xsearch - 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. GameBoard.Pieces.Length - 1 do
                            if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> GameBoard.Pieces.[q].Color
                                        then GameBoard.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
        | 3 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch + 1
                    xsearch <- xsearch - 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. GameBoard.Pieces.Length - 1 do
                            if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> GameBoard.Pieces.[q].Color
                                        then GameBoard.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
    ]
    Captures

let CheckForKnightMoves (piece:Piece) = 
    let OpenSquares : List<(int * int)> = [
    for i in 0 .. 7 do
        let mutable xsearch = fst piece.Coord
        let mutable ysearch = snd piece.Coord
        match i with
        | 0 -> // Up and Right
            ysearch <- ysearch + 2
            xsearch <- xsearch + 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 1 -> // Right and Up
            ysearch <- ysearch + 1
            xsearch <- xsearch + 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 2 -> // Right and Down
            ysearch <- ysearch - 1
            xsearch <- xsearch + 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 3 -> // Down and Right
            ysearch <- ysearch - 2
            xsearch <- xsearch + 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 4 -> // Down and Left
            ysearch <- ysearch - 2
            xsearch <- xsearch - 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 5 -> // Left and Down
            ysearch <- ysearch - 1
            xsearch <- xsearch - 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 6 -> // Left and Up
            ysearch <- ysearch + 1
            xsearch <- xsearch - 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 7 -> // Up and Left
            ysearch <- ysearch + 2
            xsearch <- xsearch - 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
    ]
    OpenSquares

let CheckForKnightCaptures (piece:Piece) = 
    let Captures : List<(Piece)> = [
    for i in 0 .. 7 do
        let mutable xsearch = fst piece.Coord
        let mutable ysearch = snd piece.Coord
        match i with
        | 0 -> // Up and Right
            ysearch <- ysearch + 2
            xsearch <- xsearch + 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. GameBoard.Pieces.Length - 1 do
                        if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> GameBoard.Pieces.[q].Color
                                    then GameBoard.Pieces.[q]
        | 1 -> // Right and Up
            ysearch <- ysearch + 1
            xsearch <- xsearch + 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. GameBoard.Pieces.Length - 1 do
                        if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> GameBoard.Pieces.[q].Color
                                    then GameBoard.Pieces.[q]
        | 2 -> // Right and Down
            ysearch <- ysearch - 1
            xsearch <- xsearch + 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. GameBoard.Pieces.Length - 1 do
                        if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> GameBoard.Pieces.[q].Color
                                    then GameBoard.Pieces.[q]
        | 3 -> // Down and Right
            ysearch <- ysearch - 2
            xsearch <- xsearch + 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. GameBoard.Pieces.Length - 1 do
                        if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> GameBoard.Pieces.[q].Color
                                    then GameBoard.Pieces.[q]
        | 4 -> // Down and Left
            ysearch <- ysearch - 2
            xsearch <- xsearch - 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. GameBoard.Pieces.Length - 1 do
                        if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> GameBoard.Pieces.[q].Color
                                    then GameBoard.Pieces.[q]
        | 5 -> // Left and Down
            ysearch <- ysearch - 1
            xsearch <- xsearch - 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. GameBoard.Pieces.Length - 1 do
                        if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> GameBoard.Pieces.[q].Color
                                    then GameBoard.Pieces.[q]
        | 6 -> // Left and Up
            ysearch <- ysearch + 1
            xsearch <- xsearch - 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. GameBoard.Pieces.Length - 1 do
                        if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> GameBoard.Pieces.[q].Color
                                    then GameBoard.Pieces.[q]
        | 7 -> // Up and Left
            ysearch <- ysearch + 2
            xsearch <- xsearch - 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. GameBoard.Pieces.Length - 1 do
                        if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> GameBoard.Pieces.[q].Color
                                    then GameBoard.Pieces.[q]
    ]
    Captures

let CheckForPawnMoves (piece:Piece) = 
    let OpenSquares : List<(int * int)> = [
        let mutable xsearch = fst piece.Coord
        let mutable ysearch = snd piece.Coord
        match piece.Color with
        | White ->   
            if snd piece.Coord = 2
                then 
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch + 2)
                        then ()
                        else
                            if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch + 1)
                                then ()
                                else
                                    (xsearch, ysearch + 2)
            if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch + 1)
                then ()
                else
                    (xsearch, ysearch + 1)
        | Black ->
            if snd piece.Coord = 7
                then 
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch - 2)
                        then ()
                        else
                            if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch - 1)
                                then ()
                                else
                                    (xsearch, ysearch - 2)
            if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch - 1)
                then ()
                else
                    (xsearch, ysearch - 1)
    ]
    OpenSquares

let CheckForPawnCaptures (piece:Piece) = 
    let Captures : List<(Piece)> = [
    for it in 0 .. 1 do
        let mutable xsearch = fst piece.Coord
        let mutable ysearch = snd piece.Coord
        match it with
        | 0 ->
                if piece.Color = White
                    then ysearch <- ysearch + 1
                    else ysearch <- ysearch - 1
                xsearch <- xsearch + 1
                if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. GameBoard.Pieces.Length - 1 do
                            if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> GameBoard.Pieces.[q].Color
                                        then GameBoard.Pieces.[q]
        | 1 ->
                if piece.Color = Black
                    then ysearch <- ysearch + 1
                    else ysearch <- ysearch - 1
                xsearch <- xsearch - 1
                if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. GameBoard.Pieces.Length - 1 do
                        if GameBoard.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> GameBoard.Pieces.[q].Color
                                    then GameBoard.Pieces.[q]
    ]
    Captures
            
let FormatMove (piece:Piece) (coord:(int * int)) = 
    let output = (CoordinateToSquare piece.Coord + ">" + CoordinateToSquare coord)
    output

let FormatCapture (movingPiece:Piece) (capturedPiece:Piece) =
    let output = (CoordinateToSquare movingPiece.Coord + ">" + CoordinateToSquare capturedPiece.Coord)
    output

let CheckForAvailableMoves (x:Piece) =
    match x.PieceType with
    | Rook -> CheckForHorizontalMoves 8 x |> List.map (FormatMove x)
    | Knight -> CheckForKnightMoves x |> List.map (FormatMove x)
    | Bishop -> CheckForDiagonalMoves 8 x |> List.map (FormatMove x)
    | Queen -> List.append (CheckForHorizontalMoves 8 x) (CheckForDiagonalMoves 8 x) |> List.map (FormatMove x)
    | King -> List.append (CheckForHorizontalMoves 2 x) (CheckForDiagonalMoves 2 x) |> List.map (FormatMove x)
    | Pawn -> CheckForPawnMoves x |> List.map (FormatMove x)

let CheckForAvailableCaptures (x:Piece) =
    match x.PieceType with
    | Rook -> CheckForHorizontalCaptures 8 x |> List.map (FormatCapture x)
    | Knight -> CheckForKnightCaptures x |> List.map (FormatCapture x)
    | Bishop -> CheckForDiagonalCaptures 8 x |> List.map (FormatCapture x)
    | Queen -> List.append (CheckForHorizontalCaptures 8 x) (CheckForDiagonalCaptures 8 x) |> List.map (FormatCapture x)
    | King -> List.append (CheckForHorizontalCaptures 2 x) (CheckForDiagonalCaptures 2 x) |> List.map (FormatCapture x)
    | Pawn -> CheckForPawnCaptures x |> List.map (FormatCapture x)

let GetAllPossibleMoves (boardState:Board) (forSide:Side) = 
    match forSide with
    | White ->
        let WhitePieces = [
        for i in 0 .. boardState.Pieces.Length - 1 do
            if boardState.Pieces.[i].Color = White
                then boardState.Pieces.[i]
        ]

        let AllPossibleMoves = [
        for i in 0 .. WhitePieces.Length - 1 do
            CheckForAvailableMoves WhitePieces.[i]
        ]

        AllPossibleMoves
    | Black ->
        let BlackPieces = [
            for i in 0 .. boardState.Pieces.Length - 1 do
                if boardState.Pieces.[i].Color = Black
                    then boardState.Pieces.[i]
            ]

        let AllPossibleMoves = [
        for i in 0 .. BlackPieces.Length - 1 do
            CheckForAvailableMoves BlackPieces.[i]
        ]

        AllPossibleMoves

let GetAllPossibleCaptures (boardState:Board) (forSide:Side) = 
    match forSide with
    | White ->
        let WhitePieces = [
        for i in 0 .. boardState.Pieces.Length - 1 do
            if boardState.Pieces.[i].Color = White
                then boardState.Pieces.[i]
        ]

        let AllPossibleCaptures = [
        for i in 0 .. WhitePieces.Length - 1 do
            CheckForAvailableCaptures WhitePieces.[i]
        ]

        AllPossibleCaptures
    | Black ->
        let BlackPieces = [
            for i in 0 .. boardState.Pieces.Length - 1 do
                if boardState.Pieces.[i].Color = Black
                    then boardState.Pieces.[i]
            ]

        let AllPossibleCaptures = [
        for i in 0 .. BlackPieces.Length - 1 do
            CheckForAvailableCaptures BlackPieces.[i]
        ]

        AllPossibleCaptures

// GetAllPossibleMoves and GetAllPossibleCaptures return a list of lists with some empty lists included
// You can pipe them into List.concat to combine all the sublists into one list of values



let MakeMove (input:String) (board:Board) = 
    let pieceSquare = input.Split('>').[0] |> SquareToCoordinate
    let targetSquare = input.Split('>').[1] |> SquareToCoordinate

    let movingPiece = board.Pieces |> List.find (fun  x -> x.Coord = pieceSquare)
    let returnBoard = (
        if board.Pieces |> List.map (fun x -> x.Coord) |> List.contains targetSquare
            then
                let capturedPiece = board.Pieces |> List.find (fun  x -> x.Coord = targetSquare)
                let newList = board.Pieces |> List.except [capturedPiece; movingPiece] |> List.append [Piece(movingPiece.Color, movingPiece.PieceType, targetSquare)]
                match board.Turn with
                | White -> Board(newList, Black)
                | Black -> Board(newList, White)
            else
                let newList = board.Pieces |> List.except [movingPiece] |> List.append [Piece(movingPiece.Color, movingPiece.PieceType, targetSquare)]
                match board.Turn with
                | White -> Board(newList, Black)
                | Black -> Board(newList, White)
    )
    returnBoard

let IsKingAttacked (board:Board) (forSide:Side) =
    match forSide with
    | White ->
        let whiteKing = board.Pieces |> List.filter (fun x -> x.PieceType = King) |> List.find (fun x -> x.Color = White)
        let AllPossibleCaptures:List<String> = GetAllPossibleCaptures board Black |> List.concat
        if AllPossibleCaptures |> List.map (fun x -> x.Split('>').[1]) |> List.contains (CoordinateToSquare whiteKing.Coord)
        then 
            true
            
        else 
            false
            
    | Black ->
        let blackKing = board.Pieces |> List.filter (fun x -> x.PieceType = King) |> List.find (fun x -> x.Color = White)
        let AllPossibleCaptures:List<String> = GetAllPossibleCaptures board White |> List.concat
        if AllPossibleCaptures |> List.map (fun x -> x.Split('>').[1]) |> List.contains (CoordinateToSquare blackKing.Coord)
        then   
            true
        else 
            false

let IsMoveLegal (move:String) (board:Board) =
    let hypotheticalBoard = MakeMove move board
    if IsKingAttacked hypotheticalBoard board.Turn = true
    then false
    else true

let FindAllLegalMoves (board:Board) =
    let l1 = GetAllPossibleMoves board board.Turn |> List.concat
    let l2 = GetAllPossibleCaptures board board.Turn |> List.concat
    let l3 = [l1; l2] |> List.concat
    let test = l3 |> List.filter (fun x -> (IsMoveLegal x board) = true)
    test

//GameBoard <- MakeMove "d2d4" GameBoard

printfn "%A" GameBoard.Turn

printfn "\nKing in check: %A\n" (IsKingAttacked GameBoard GameBoard.Turn)

printfn "Possible Moves: %A" (FindAllLegalMoves GameBoard)
