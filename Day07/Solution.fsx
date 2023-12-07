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
        | 'A' -> 1
        | 'K' -> 2
        | 'Q' -> 3
        | 'J' -> 4
        | 'T' -> 5
        | '9' -> 6
        | '8' -> 7
        | '7' -> 8
        | '6' -> 9
        | '5' -> 10
        | '4' -> 11
        | '3' -> 12
        | '2' -> 13
        | _ -> failwith "unexpected card"

    Console.WriteLine $"scoring {s}"
    let chunks =
        s
        |> Seq.chunkBy id
        |>> fun (c, a) -> scoreCard c, Seq.length a
    //chunks
    //|> Seq.iter (fun (c, a) -> Console.WriteLine ($"Chunk {c} length {a}"))
    //"todo"


let part1 (input: Input seq) =
    input
    |> Seq.sortBy ((fun h -> h.Hand) >> scoreFunction)
    |> Seq.mapi (fun i hand -> (i + 1) * hand.Bid)
    |> Seq.sum
    |> sprintf "%i"

let part2 input = 
    "todo"

module Tests =
    let private tests = 
        [
            fun () ->
                ["23333";"433AQ"]
                |> List.sortBy scoreFunction
                |> function 
                    | ["433AQ";"23333"] -> Ok ()
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
