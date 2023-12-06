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
    Time: int
    Distance: int
}

let parseInput (input:string seq) =
    input
    |>> (String.split [|":"|] >> Seq.tail >> Seq.head)
    |>> String.split [|" "|]
    |> Seq.map (Seq.where (fun s -> s <> "") >> Seq.map (fun s -> (Int32.Parse(s))))
    |> List.ofSeq
    |> function
        | [timeRow;distRow] ->
            let distList = distRow |> List.ofSeq
            timeRow
            |> Seq.mapi (fun i t -> {Time = t; Distance = distList[i]})
        | _ -> failwith "unexpected input"

let readFile fileName =
    try
        File.ReadLines fileName
        |> parseInput
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let distances (time: int): Int64 seq =
    seq {1..time - 1}
    |>> int64
    |>> fun chargeTime -> chargeTime * (int64 time - chargeTime)

let part1 (input: Input seq) =
    Console.WriteLine ($"{Seq.length input} races")
    input
    |>> (fun race ->
        distances race.Time
        |> Seq.where ((<) race.Distance)
    )
    |>> (Seq.length >> int64)
    |> Seq.fold (*) 1L
    |> sprintf "%i"

let part2 input = 
    "todo"

module Tests =
    let private tests = 
        [
            fun () -> 
                distances 7 |> List.ofSeq |> List.sort
                |> function
                | [6L;6L;10L;10L;12L;12L] -> Ok ()
                | other -> Error $"{other}"
            fun () -> 
                [|2;3;2|] |> Seq.fold (*) 1
                |> function
                | 12 -> Ok ()
                | other -> Error $"{other}"
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
