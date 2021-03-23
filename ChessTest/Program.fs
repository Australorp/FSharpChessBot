// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

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

type Direction = 
    | N
    | S
    | E
    | W
    | NW
    | NE
    | SW
    | SE

type Tile = int * int

type Piece(pieceColor:Side, typeOfPiece: PieceTypes, coord: (int * int)) =
    member _.Color = pieceColor
    member _.PieceType = typeOfPiece
    member _.Coord = coord

type Board(pieces:List<Piece>, turn :Side) =
    member _.Pieces = pieces
    member _.Turn = turn

type Move = 
    | AvailableMove of Tile
    | AvailableCapture of Piece


// End of type declarations

let StartingFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
let FriedLiverFEN = "r1bqk2r/pppp1ppp/2n2n2/2b1p1N1/2B1P3/8/PPPP1PPP/RNBQK2R w KQkq - 6 5"
let TestingFEN = "r1b1kbnr/ppp1pppp/2n5/4q3/8/2N2N2/PPPP1PPP/R1BQKB1R w KQkq - 4 5"

let chessTiles = 
    [|
        for x in 1..8 do
            for y in 1..8 do
                x, y
    |]

let doesTileExist (x, y) = Array.contains (x, y) chessTiles

let toChessNotation (coord:(int * int)) =
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

let isCapturable (attacker: Piece) (piece: Piece) = 
    match attacker, piece with
    | a, p when p.PieceType = King -> false
    | a, p when a.Color = p.Color -> false
    | _ -> true

let tileInDirection direction (x, y) =
    let tile = 
        match direction with
        | N -> x, y + 1
        | S -> x, y - 1
        | E -> x + 1, y
        | W -> x - 1, y
        | NE -> x + 1, y + 1
        | NW -> x - 1, y + 1
        | SE -> x + 1, y - 1
        | SW -> x - 1, y - 1

    chessTiles
    |> Array.tryFind (fun x -> x = tile)

let allDirectionalMoves (allPieces: Piece list) (pieceToMove: Piece) direction =
    let pieceOnTile tile =
        allPieces
        |> List.tryFind (fun x -> x.Coord = tile)

    /// recursively scans directions to return list of allowed moves/captures
    let rec scanDirection from collectedMoves =
        match tileInDirection direction from with
        | None -> collectedMoves
        | Some tile ->
            match pieceOnTile tile with
            | None -> 
                let move = AvailableMove tile
                scanDirection tile (move::collectedMoves)
            | Some piece ->
                if isCapturable pieceToMove piece then
                    let capture = AvailableCapture piece
                    scanDirection tile (capture::collectedMoves)
                else 
                    collectedMoves
    
    scanDirection pieceToMove.Coord [] // seed the recursive function with an empty starting list

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
        boardState.Pieces
        |> List.map (fun piece -> 
        match piece.Color with
        | White ->
            match piece.PieceType with
            | Rook -> 5
            | Knight -> 3
            | Bishop -> 3
            | Queen -> 9
            | King -> 0
            | Pawn -> 1
            
        | Black ->
            match piece.PieceType with
            | Rook -> -5
            | Knight -> -3
            | Bishop -> -3
            | Queen -> -9
            | King -> 0
            | Pawn -> -1
            )
    ]
    materialBalance |> List.concat |> List.sum

let mutable GameBoard = Board((ReadFEN FriedLiverFEN), White)

let CheckForHorizontalMoves (range:int) (piece:Piece) (board:Board) = 
    let OpenSquares : List<(int * int)> = [
    for it in 0 .. 3 do
        let mutable xsearch = fst piece.Coord
        let mutable ysearch = snd piece.Coord
        match it with
        | 0 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch + 1
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
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
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
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
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
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
                   if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
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

let CheckForHorizontalCaptures (range:int) (piece:Piece) (board:Board) = 
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
                            for q in 0 .. board.Pieces.Length - 1 do
                                if board.Pieces.[q].Coord = (xsearch, ysearch)
                                    then 
                                        if piece.Color <> board.Pieces.[q].Color
                                            then board.Pieces.[q]
                                        i <- range + 99
                    i <- i + 1
        | 1 -> let mutable i = 1
               while (i < range) do
                    xsearch <- xsearch + 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. board.Pieces.Length - 1 do
                            if board.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> board.Pieces.[q].Color
                                        then board.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
        | 2 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch - 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. board.Pieces.Length - 1 do
                            if board.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> board.Pieces.[q].Color
                                        then board.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
        | 3 -> let mutable i = 1
               while (i < range) do
                    xsearch <- xsearch - 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. board.Pieces.Length - 1 do
                            if board.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> board.Pieces.[q].Color
                                        then board.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
    ]
    Captures

let CheckForDiagonalMoves (range:int) (piece:Piece) (board:Board) = 
    let OpenSquares : List<(int * int)> = [
    for it in 0 .. 3 do
        let mutable xsearch = fst piece.Coord
        let mutable ysearch = snd piece.Coord
        match it with
        | 0 ->  let mutable i = 1
                while (i < range) do
                    ysearch <- ysearch + 1
                    xsearch <- xsearch + 1
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
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
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
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
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
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
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
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

let CheckForDiagonalCaptures (range:int) (piece:Piece) (board:Board) = 
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
                            for q in 0 .. board.Pieces.Length - 1 do
                                if board.Pieces.[q].Coord = (xsearch, ysearch)
                                    then 
                                        if piece.Color <> board.Pieces.[q].Color
                                            then board.Pieces.[q]
                                        i <- range + 99
                    i <- i + 1
        | 1 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch - 1
                    xsearch <- xsearch + 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. board.Pieces.Length - 1 do
                            if board.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> board.Pieces.[q].Color
                                        then board.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
        | 2 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch - 1
                    xsearch <- xsearch - 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. board.Pieces.Length - 1 do
                            if board.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> board.Pieces.[q].Color
                                        then board.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
        | 3 -> let mutable i = 1
               while (i < range) do
                    ysearch <- ysearch + 1
                    xsearch <- xsearch - 1
                    if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                    then
                        for q in 0 .. board.Pieces.Length - 1 do
                            if board.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> board.Pieces.[q].Color
                                        then board.Pieces.[q]
                                    i <- range + 99
                    i <- i + 1
    ]
    Captures

let CheckForKnightMoves (piece:Piece) (board:Board) = 
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
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 1 -> // Right and Up
            ysearch <- ysearch + 1
            xsearch <- xsearch + 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 2 -> // Right and Down
            ysearch <- ysearch - 1
            xsearch <- xsearch + 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 3 -> // Down and Right
            ysearch <- ysearch - 2
            xsearch <- xsearch + 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 4 -> // Down and Left
            ysearch <- ysearch - 2
            xsearch <- xsearch - 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 5 -> // Left and Down
            ysearch <- ysearch - 1
            xsearch <- xsearch - 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 6 -> // Left and Up
            ysearch <- ysearch + 1
            xsearch <- xsearch - 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
        | 7 -> // Up and Left
            ysearch <- ysearch + 2
            xsearch <- xsearch - 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch)
                        then ()
                        else
                            (xsearch, ysearch)
    ]
    OpenSquares

let CheckForKnightCaptures (piece:Piece) (board:Board) = 
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
                    for q in 0 .. board.Pieces.Length - 1 do
                        if board.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> board.Pieces.[q].Color
                                    then board.Pieces.[q]
        | 1 -> // Right and Up
            ysearch <- ysearch + 1
            xsearch <- xsearch + 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. board.Pieces.Length - 1 do
                        if board.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> board.Pieces.[q].Color
                                    then board.Pieces.[q]
        | 2 -> // Right and Down
            ysearch <- ysearch - 1
            xsearch <- xsearch + 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. board.Pieces.Length - 1 do
                        if board.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> board.Pieces.[q].Color
                                    then board.Pieces.[q]
        | 3 -> // Down and Right
            ysearch <- ysearch - 2
            xsearch <- xsearch + 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. board.Pieces.Length - 1 do
                        if board.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> board.Pieces.[q].Color
                                    then board.Pieces.[q]
        | 4 -> // Down and Left
            ysearch <- ysearch - 2
            xsearch <- xsearch - 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. board.Pieces.Length - 1 do
                        if board.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> board.Pieces.[q].Color
                                    then board.Pieces.[q]
        | 5 -> // Left and Down
            ysearch <- ysearch - 1
            xsearch <- xsearch - 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. board.Pieces.Length - 1 do
                        if board.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> board.Pieces.[q].Color
                                    then board.Pieces.[q]
        | 6 -> // Left and Up
            ysearch <- ysearch + 1
            xsearch <- xsearch - 2
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. board.Pieces.Length - 1 do
                        if board.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> board.Pieces.[q].Color
                                    then board.Pieces.[q]
        | 7 -> // Up and Left
            ysearch <- ysearch + 2
            xsearch <- xsearch - 1
            if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. board.Pieces.Length - 1 do
                        if board.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> board.Pieces.[q].Color
                                    then board.Pieces.[q]
    ]
    Captures

let CheckForPawnMoves (piece:Piece) (board:Board) = 
    let OpenSquares : List<(int * int)> = [
        let mutable xsearch = fst piece.Coord
        let mutable ysearch = snd piece.Coord
        match piece.Color with
        | White ->   
            if snd piece.Coord = 2
                then 
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch + 2)
                        then ()
                        else
                            if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch + 1)
                                then ()
                                else
                                    (xsearch, ysearch + 2)
            if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch + 1)
                then ()
                else
                    (xsearch, ysearch + 1)
        | Black ->
            if snd piece.Coord = 7
                then 
                    if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch - 2)
                        then ()
                        else
                            if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch - 1)
                                then ()
                                else
                                    (xsearch, ysearch - 2)
            if board.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch - 1)
                then ()
                else
                    (xsearch, ysearch - 1)
    ]
    OpenSquares

let CheckForPawnCaptures (piece:Piece) (board:Board) = 
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
                        for q in 0 .. board.Pieces.Length - 1 do
                            if board.Pieces.[q].Coord = (xsearch, ysearch)
                                then 
                                    if piece.Color <> board.Pieces.[q].Color
                                        then board.Pieces.[q]
        | 1 ->
                if piece.Color = Black
                    then ysearch <- ysearch + 1
                    else ysearch <- ysearch - 1
                xsearch <- xsearch - 1
                if ysearch > 0 && ysearch < 9 && xsearch > 0 && xsearch < 9
                then
                    for q in 0 .. board.Pieces.Length - 1 do
                        if board.Pieces.[q].Coord = (xsearch, ysearch)
                            then 
                                if piece.Color <> board.Pieces.[q].Color
                                    then board.Pieces.[q]
    ]
    Captures
            
let FormatMove (piece:Piece) (coord:(int * int)) = 
    let output = (toChessNotation piece.Coord + ">" + toChessNotation coord)
    output

let FormatCapture (movingPiece:Piece) (capturedPiece:Piece) =
    let output = (toChessNotation movingPiece.Coord + ">" + toChessNotation capturedPiece.Coord)
    output

let CheckForAvailableMoves (x:Piece) (board:Board) =
    match x.PieceType with
    | Rook -> CheckForHorizontalMoves 8 x board |> List.map (FormatMove x)
    | Knight -> CheckForKnightMoves x board |> List.map (FormatMove x)
    | Bishop -> CheckForDiagonalMoves 8 x board |> List.map (FormatMove x)
    | Queen -> List.append (CheckForHorizontalMoves 8 x board) (CheckForDiagonalMoves 8 x board) |> List.map (FormatMove x)
    | King -> List.append (CheckForHorizontalMoves 2 x board) (CheckForDiagonalMoves 2 x board) |> List.map (FormatMove x)
    | Pawn -> CheckForPawnMoves x board |> List.map (FormatMove x)

let CheckForAvailableCaptures (x:Piece) (board:Board) =
    match x.PieceType with
    | Rook -> CheckForHorizontalCaptures 8 x board |> List.map (FormatCapture x)
    | Knight -> CheckForKnightCaptures x board |> List.map (FormatCapture x)
    | Bishop -> CheckForDiagonalCaptures 8 x board |> List.map (FormatCapture x)
    | Queen -> List.append (CheckForHorizontalCaptures 8 x board) (CheckForDiagonalCaptures 8 x board) |> List.map (FormatCapture x)
    | King -> List.append (CheckForHorizontalCaptures 2 x board) (CheckForDiagonalCaptures 2 x board) |> List.map (FormatCapture x)
    | Pawn -> CheckForPawnCaptures x board |> List.map (FormatCapture x)

let GetAllPossibleMoves (boardState:Board) (forSide:Side) = 
    match forSide with
    | White ->
        let WhitePieces =
            boardState.Pieces
            |> List.filter (fun piece -> piece.Color = White)

        let AllPossibleMoves = 
            WhitePieces
            |> Seq.mapParallel 8 (fun piece -> CheckForAvailableMoves piece boardState)
            |> List.ofArray

        AllPossibleMoves
    | Black ->
        let BlackPieces =
            boardState.Pieces
            |> List.filter (fun piece -> piece.Color = Black)

        let AllPossibleMoves = 
            BlackPieces
            |> Seq.mapParallel 8 (fun piece -> CheckForAvailableMoves piece boardState)
            |> List.ofArray

        AllPossibleMoves

let GetAllPossibleCaptures (boardState:Board) (forSide:Side) = 
    match forSide with
    | White ->
        let WhitePieces =
            boardState.Pieces
            |> List.filter (fun piece -> piece.Color = White)

        let AllPossibleCaptures = 
            WhitePieces
            |> Seq.mapParallel 8 (fun piece -> CheckForAvailableCaptures piece boardState)
            |> List.ofArray

        AllPossibleCaptures
    | Black ->
        let BlackPieces =
            boardState.Pieces
            |> List.filter (fun piece -> piece.Color = Black)

        let AllPossibleCaptures = 
            BlackPieces
            |> Seq.mapParallel 8 (fun piece -> CheckForAvailableCaptures piece boardState)
            |> List.ofArray

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
        if AllPossibleCaptures |> List.map (fun x -> x.Split('>').[1]) |> List.contains (toChessNotation whiteKing.Coord)
        then 
            true
            
        else 
            false
            
    | Black ->
        let blackKing = board.Pieces |> List.filter (fun x -> x.PieceType = King) |> List.find (fun x -> x.Color = Black)
        let AllPossibleCaptures:List<String> = GetAllPossibleCaptures board White |> List.concat
        if AllPossibleCaptures |> List.map (fun x -> x.Split('>').[1]) |> List.contains (toChessNotation blackKing.Coord)
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
    let output = l3 |> List.filter (fun x -> (IsMoveLegal x board) = true)
    output

let FindAllLegalCaptures (board:Board) =
    let l1 = GetAllPossibleCaptures board board.Turn |> List.concat
    let output = l1 |> List.filter (fun x -> (IsMoveLegal x board) = true)
    output


let rec alphaBetaMax alpha beta depthLeft board (moves:List<String>) = 
    if depthLeft = 0
        then
            (CalculateMaterialBalance board), moves
        else
            let allMoves = FindAllLegalMoves board 
            let mutable output:int = alpha
            let mutable moveList = []
            for move in allMoves do
                let score = alphaBetaMin alpha beta (depthLeft - 1) (MakeMove move board) (move::moves)
                if fst score >= beta
                    then 
                        output <- beta
                        moveList <- moves
                if fst score > alpha
                    then 
                        output <- fst score
                        moveList <- snd score |> List.rev
            output, moveList
and alphaBetaMin alpha beta depthLeft board (moves:List<String>) = 
    if depthLeft = 0
        then
            (CalculateMaterialBalance board), moves
        else
            let allMoves = FindAllLegalMoves board 
            let mutable output:int = beta
            let mutable moveList = moves
            for move in allMoves do
                printfn "%A" (move::moves)
                let score = alphaBetaMax alpha beta (depthLeft - 1) (MakeMove move board) (move::moves)
                if fst score <= alpha
                    then 
                        output <- alpha
                        moveList <- moves
                if fst score < beta
                    then 
                        output <- fst score
                        moveList <- snd score |> List.rev
            output, moveList


let analyzeBoard board depth =
    
    let rec bestMove depth moves (board:Board) =

        let valueForSide eval =
            match board.Turn with
            | Black -> eval * -1
            | White -> eval
        
        (*
        let allCaptures = FindAllLegalCaptures board

        let allMoves = (
            if allCaptures.IsEmpty
                then 
                    FindAllLegalMoves board
                else 
                    allCaptures
            )
        *)

        let allMoves = FindAllLegalMoves board

        if Seq.isEmpty allMoves then
            0, []
        else
            let initialMoves =
                allMoves
                |> Seq.mapParallel 8 (fun move -> move, MakeMove move board)
                |> Seq.toArray
                |> Seq.mapParallel 8 (fun (move, board) -> move, board, (CalculateMaterialBalance board))
                |> Seq.toArray
                |> Seq.sortByDescending (fun (move, board, eval) -> valueForSide eval)

            let bestMoves =
                match depth with
                | 0 | 1 -> 
                    initialMoves
                    |> Seq.map (fun (move, board, eval) -> move, board, eval, (move::moves))
                    |> Array.ofSeq
                    //|> Seq.map (fun (move, board, eval) -> eval)
                | depth -> 
                    initialMoves
                    |> Seq.map (fun (move, board, eval) -> 
                        let eval, moves = bestMove (depth - 1) (move::moves) board
                        move, board, eval, moves)
                    //|> Seq.map (fun (move, board, eval) -> async { return move, board, bestMove (depth - 1) board })
                    |> Array.ofSeq
                    //|> Async.runParallelSize 3
                    //|> Async.RunSynchronously
                    |> Array.sortByDescending (fun (move, board, eval, moves) -> valueForSide eval)

            let firstMove, board, eval, moves = Seq.head bestMoves
            
            //printfn "Move: %A\n FEN: %A\n Eval: %A\n" firstMove (ComputeFEN board) eval

            //let test = bestMoves |> Seq.iter (fun (move, board, eval) -> printfn "Move: %A\n Board: %A\nEval: %A\nDepth: %A\n" move (ComputeFEN board) eval depth)

            eval, moves

    let eval, moves = bestMove board [] depth
    eval, List.rev moves

let rec AnalysisTest (board:Board) (depth:int) (forSide:Side) = 
    if (FindAllLegalMoves board).IsEmpty
        then 
            match forSide with
            | White ->
                -99
            | Black ->
                99
        else
            let mutable BestValue = 0
            for move in FindAllLegalMoves board do
                let resultingBoard = MakeMove move board
                let boardEval = CalculateMaterialBalance resultingBoard

                //let text = "Move: " + move + "   Material Balance " + (CalculateMaterialBalance resultingBoard).ToString() + "   FEN Position " + (ComputeFEN resultingBoard) + "             "
                //File.AppendAllText(file, text)

                

                match board.Turn with
                | White ->
                    if boardEval > BestValue
                    then 
                        BestValue <- boardEval
                        //printfn "Move: %A\n Material Balance: %A\n FEN: %A\n\n\n" move boardEval (ComputeFEN resultingBoard)
                | Black ->
                    if boardEval < BestValue
                    then 
                        BestValue <- boardEval
                        //printfn "Move: %A\n Material Balance: %A\n FEN: %A\n\n\n" move boardEval (ComputeFEN resultingBoard)

                if depth > 1
                    then
                        AnalysisTest resultingBoard (depth - 1) forSide |> ignore

            
            BestValue

printfn "%A" (alphaBetaMax -99 99 3 GameBoard [])

//printfn "Final Result: %A" (analyzeBoard 4 GameBoard)

//printfn "Legal moves for Black: %A" (FindAllLegalMoves GameBoard)
