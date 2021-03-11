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

type Piece(pieceColor, typeOfPiece: PieceTypes, coord: (int * int)) =
    member _.Color = pieceColor
    member _.PieceType = typeOfPiece
    member _.Coord = coord

type Board(pieces:List<Piece>, turn :Side) =
    member _.Pieces = pieces
    member _.Turn = turn


// End of type declarations

let StartingFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
let FriedLiverFEN = "r1bqk2r/pppp1ppp/2n2n2/2b1p1N1/2B1P3/8/PPPP1PPP/RNBQK2R w KQkq - 6 5"
let TestingFEN = "2b1kQ1r/pp1Nbppp/2n2n2/2q1p1P1/NB1PP3/8/PPP2P1P/RrB1K2R w KQk - 6 5"

let MatchPiece x (coord:(int * int)) =
    match x with
    | "R" -> Some("W", Rook, coord)
    | "N" -> Some("W", Knight, coord)
    | "B" -> Some("W", Bishop, coord)
    | "Q" -> Some("W", Queen, coord)
    | "K" -> Some("W", King, coord)
    | "P" -> Some("W", Pawn, coord)
    | "r" -> Some("B", Rook, coord)
    | "n" -> Some("B", Knight, coord)
    | "b" -> Some("B", Bishop, coord)
    | "q" -> Some("B", Queen, coord)
    | "k" -> Some("B", King, coord)
    | "p" -> Some("B", Pawn, coord)
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
                    | "W" -> 
                        match boardState.Pieces.[p].PieceType with
                        | Rook -> testString <- testString + "R"
                        | Knight -> testString <- testString + "N"
                        | Bishop -> testString <- testString + "B"
                        | Queen -> testString <- testString + "Q"
                        | King -> testString <- testString + "K"
                        | Pawn -> testString <- testString + "P" 
                    | "B" ->
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
                    let test = MatchPiece (c.[x].ToString()) (xcount + 1, ycount) |> Option.map Piece
                    match test with
                    | Some(x) -> x
                    | None -> ()
                    xcount <- xcount + 1
            
        ycount <- ycount + 1
    ]
    AllPieces

let mutable GameBoard = Board((ReadFEN StartingFEN), White)

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
        | "W" ->   
            if snd piece.Coord = 2
                then 
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch + 2)
                        then ()
                        else
                            (xsearch, ysearch + 2)
            if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch + 1)
                then ()
                else
                    (xsearch, ysearch + 1)
        | "B" ->
            if snd piece.Coord = 7
                then 
                    if GameBoard.Pieces |> List.map (fun q -> q.Coord) |> List.contains (xsearch, ysearch - 2)
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
                if piece.Color = "W"
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
                if piece.Color = "W"
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
            



let CheckForAvailableMoves (x:Piece) =
    match x.PieceType with
    | Rook -> CheckForHorizontalMoves 8 x
    | Knight -> CheckForKnightMoves x
    | Bishop -> CheckForDiagonalMoves 8 x
    | Queen -> List.append (CheckForHorizontalMoves 8 x) (CheckForDiagonalMoves 8 x) 
    | King -> List.append (CheckForHorizontalMoves 2 x) (CheckForDiagonalMoves 2 x)
    | Pawn -> CheckForPawnMoves x

let CheckForAvailableCaptures (x:Piece) =
    match x.PieceType with
    | Rook -> CheckForHorizontalCaptures 8 x
    | Knight -> CheckForKnightCaptures x
    | Bishop -> CheckForDiagonalCaptures 8 x
    | Queen -> List.append (CheckForHorizontalCaptures 8 x) (CheckForDiagonalCaptures 8 x)
    | King -> List.append (CheckForHorizontalCaptures 2 x) (CheckForDiagonalCaptures 2 x)
    | Pawn -> CheckForPawnCaptures x

let GetAllPossibleMoves (boardState:Board) = 
    match boardState.Turn with
    | White ->
        let WhitePieces = [
        for i in 0 .. boardState.Pieces.Length - 1 do
            if boardState.Pieces.[i].Color = "W"
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
                if boardState.Pieces.[i].Color = "W"
                    then boardState.Pieces.[i]
            ]

        let AllPossibleMoves = [
        for i in 0 .. BlackPieces.Length - 1 do
            CheckForAvailableMoves BlackPieces.[i]
        ]

        AllPossibleMoves

let GetAllPossibleCaptures (boardState:Board) = 
    match boardState.Turn with
    | White ->
        let WhitePieces = [
        for i in 0 .. boardState.Pieces.Length - 1 do
            if boardState.Pieces.[i].Color = "W"
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
                if boardState.Pieces.[i].Color = "W"
                    then boardState.Pieces.[i]
            ]

        let AllPossibleCaptures = [
        for i in 0 .. BlackPieces.Length - 1 do
            CheckForAvailableCaptures BlackPieces.[i]
        ]

        AllPossibleCaptures

// GetAllPossibleMoves and GetAllPossibleCaptures return a list of lists with some empty lists included
// The end goal for these two functions would be to return one list of all possible coordinates or pieces and no empties
// Also these functions may not be super useful in their current form
// I'm just keeping them because I just wrote them and they may turn out to be helpful eventually



let MakeMove (piece:Piece) (square:(int * int)) = 
    let mutable OpenMove = true
    for q in 0 .. GameBoard.Pieces.Length - 2 do
        if GameBoard.Pieces.[q].Coord = square
            then
                OpenMove <- false
                if GameBoard.Pieces.[q] <> piece && GameBoard.Pieces.[q].Color <> piece.Color
                    then
                        let newList = GameBoard.Pieces |> List.except [GameBoard.Pieces.[q]; piece] |> List.append [Piece(piece.Color, piece.PieceType, square)]
                        match GameBoard.Turn with
                        | White -> GameBoard <- Board(newList, Black)
                        | Black -> GameBoard <- Board(newList, White)
    if OpenMove = true
        then
            let newList = GameBoard.Pieces |> List.except [piece] |> List.append [Piece(piece.Color, piece.PieceType, square)]
            match GameBoard.Turn with
            | White -> GameBoard <- Board(newList, Black)
            | Black -> GameBoard <- Board(newList, White)

// I've only use the MakeMove function manually so far as a testing feature,
// but eventually the piece to be moved and the intended square will be fed into it
// from somewhere else
            
// The CheckFor... series of functions actually handle finding legal moves and captures

// I made the GameBoard variable mutable and the MakeMove function creates and new board state, and then assigns that to
// the GameBoard variable

// In the future, analyzing a move ahead will create a hypothetical board state, which could then be fed in all the
// different analysis functions

MakeMove GameBoard.Pieces.[12] (5, 4) // Pawn e4

MakeMove GameBoard.Pieces.[20] (5, 5) // Pawn e5

printfn "%A" (ComputeFEN GameBoard)









   
            
    