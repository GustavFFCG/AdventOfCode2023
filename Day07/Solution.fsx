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

type Input = {
    Hand: string
    Bid: int
}

let parseInput s =
    s
    |> String.split [|" "|]
    |> List.ofSeq
    |> function 
        | [hand;bid] -> {Hand = hand;Bid = Int32.Parse(bid) }
        | other -> failwith $"unexpected input {other}"

let readFile fileName =
    try
        File.ReadLines fileName
        |>> parseInput
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let scoreFunction (s:string) =
    let scoreCard = function
        | 'A' -> 13
        | 'K' -> 12
        | 'Q' -> 11
        | 'J' -> 10
        | 'T' -> 9
        | '9' -> 8
        | '8' -> 7
        | '7' -> 6
        | '6' -> 5
        | '5' -> 4
        | '4' -> 3
        | '3' -> 2
        | '2' -> 1
        | _ -> failwith "unexpected card"

    let chunks =
        s
        |> Seq.sort
        |> Seq.chunkBy id
        |>> (fun (c, a) -> Seq.length a)
        |> Seq.sortDescending
        |> List.ofSeq
    let cardScores = s |> Seq.map scoreCard |> List.ofSeq
    let handScore =
        match chunks with
        | [five] -> 7
        | 4::_ -> 6
        | [3;2] -> 5
        | 3::_ -> 4
        | [2;2;1] -> 3
        | 2::_ -> 2
        | _ -> 1
    handScore::cardScores

let scoreFunction2 (s:string) =
    //Console.WriteLine $"scoring {s}"
    let scoreCard = function
        | 'A' -> 13
        | 'K' -> 12
        | 'Q' -> 11
        | 'J' -> 0
        | 'T' -> 9
        | '9' -> 8
        | '8' -> 7
        | '7' -> 6
        | '6' -> 5
        | '5' -> 4
        | '4' -> 3
        | '3' -> 2
        | '2' -> 1
        | _ -> failwith "unexpected card"

    let chunks =
        s
        |> String.replace "J" ""
        |> Seq.sort
        |> Seq.chunkBy id
        |>> (fun (c, a) -> Seq.length a)
        |> Seq.sortDescending
        |> List.ofSeq
    let cardScores = s |> Seq.map scoreCard |> List.ofSeq
    let handScore =
        match chunks with
        | [_any] -> 7
        | [] -> 7
        | [4;1] -> 6
        | [3;1] -> 6
        | [2;1] -> 6
        | [1;1] -> 6
        | [3;2] -> 5
        | [2;2] -> 5
        | [3;1;1] -> 4
        | [2;1;1] -> 4
        | [1;1;1] -> 4
        | [2;2;1] -> 3
        | [2;1;1;1] -> 2
        | [1;1;1;1] -> 2
        | [1;1;1;1;1] -> 1
        | other -> failwith $"unhandled hand {other}"
    //Console.WriteLine $"handScore {handScore}"
    handScore::cardScores

let part1 (input: Input seq) =
    input
    |> Seq.sortBy ((fun h -> h.Hand) >> scoreFunction)
    |> Seq.mapi (fun i hand -> (i + 1) * hand.Bid)
    |> Seq.sum
    |> sprintf "%i"

let part2 input = 
    input
    |> Seq.sortBy ((fun h -> h.Hand) >> scoreFunction2)
    |> Seq.mapi (fun i hand -> (i + 1) * hand.Bid)
    |> Seq.sum
    |> sprintf "%i"

module Tests =
    let private tests = 
        [
            fun () ->
                ["23333";"433AQ"]
                |> List.sortBy scoreFunction
                |> function 
                    | ["433AQ";"23333"] -> Ok ()
                    | other -> Error $"Got {other}"
            fun () ->
                ["32T3K";"T55J5";"KK677";"KTJJT";"QQQJA" ]
                |> List.sortBy scoreFunction
                |> function 
                    | ["32T3K";"KTJJT";"KK677";"T55J5";"QQQJA" ] -> Ok ()
                    | other -> Error $"Got {other}"
            fun () ->
                ["32T3K";"T55J5";"KK677";"KTJJT";"QQQJA" ]
                |> List.sortBy scoreFunction2
                |> function 
                    | ["32T3K";"KK677";"T55J5";"QQQJA";"KTJJT" ] -> Ok ()
                    | other -> Error $"Got {other}"
            fun () ->
                ["22222";"JJJJJ"]
                |> List.sortBy scoreFunction2
                |> function 
                    | ["JJJJJ";"22222" ] -> Ok ()
                    | other -> Error $"Got {other}"
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
