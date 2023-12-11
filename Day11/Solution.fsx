#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function 
    | _::s::_ -> Some s 
    | _ -> None

let parseInput (input:string seq) =
    input
    |> Seq.mapi (fun y s ->
        s
        |> Seq.mapi (fun x c -> if c = '#' then Some (x, y) else None)
        |> Seq.choose id
    )
    |> Seq.concat

let readFile fileName =
    try
        File.ReadLines fileName
        |> parseInput
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let rec distances (points:(int*int) list) =
    match points with
        | [] -> 0
        | (originX, originY)::tail ->
            tail
            |> Seq.sumBy (fun (x, y) -> abs(x - originX) + abs(y - originY))
            |> (+) (distances tail)

let rec distances64 (points:(int64*int64) list) =
    match points with
        | [] -> 0L
        | (originX, originY)::tail ->
            tail
            |> Seq.sumBy (fun (x, y) -> abs(x - originX) + abs(y - originY))
            |> (+) (distances64 tail)

let part1 (input: (int*int) seq) =
    let blankRows =
        input
        |>> snd
        |> fun y -> seq{(Seq.min y + 1)..(Seq.max y - 1)}
        |> Seq.where (fun row -> input |> Seq.exists (fun (_x,y) -> y = row) |> not)
    let blankCols =
        input
        |>> fst
        |> fun x -> seq{(Seq.min x + 1)..(Seq.max x - 1)}
        |> Seq.where (fun col -> input |> Seq.exists (fun (x,_y) -> x = col) |> not)
    
    input
    |>> (fun (x, y) -> 
        (blankCols |> Seq.where(fun col -> col < x) |> Seq.length) + x,
        (blankRows |> Seq.where(fun row -> row < y) |> Seq.length) + y
        )
    |> List.ofSeq
    |> distances
    |> sprintf "%i"

let part2 (input: (int*int) seq) =
    let blankRows =
        input
        |>> snd
        |> fun y -> seq{(Seq.min y + 1)..(Seq.max y - 1)}
        |> Seq.where (fun row -> input |> Seq.exists (fun (_x,y) -> y = row) |> not)
    let blankCols =
        input
        |>> fst
        |> fun x -> seq{(Seq.min x + 1)..(Seq.max x - 1)}
        |> Seq.where (fun col -> input |> Seq.exists (fun (x,_y) -> x = col) |> not)
    
    input
    |>> (fun (x, y) -> 
        (blankCols |> Seq.where(fun col -> col < x) |> Seq.length) + (x * 1000000) |> int64,
        (blankRows |> Seq.where(fun row -> row < y) |> Seq.length) + (y * 1000000) |> int64
        )
    |> List.ofSeq
    |> distances64
    |> sprintf "%i"

module Tests =
    let private tests = 
        [
            fun () -> 
                [(1,2);(3,1)]
                |> distances
                |> function | 3 -> Ok () | other -> Error $"Expected 3 got {other}"
            fun () -> 
                [(1,2);(3,1);(0,0)]
                |> distances
                |> function | 10 -> Ok () | other -> Error $"Expected 10 got {other}"
        ]
    let run () =
        tests 
        |> List.fold (fun state test -> state |> Result.bind test ) (Ok ())
        |>> fun () -> "All tests Ok"

let input = fileName |>> readFile
match input with
| Some input ->
    Result.map3
        (sprintf "Successful run!\r\nTests: %s\r\nPart1: %s\r\nPart2: %s")
        (Tests.run ())
        (input |>> part1)
        (input |>> part2)
| None ->
    Tests.run () |>> sprintf "Tests only:\r\n %s"
|> function
| Ok s -> s
| Error s -> sprintf "Error: %s" s 
|> Console.WriteLine 
