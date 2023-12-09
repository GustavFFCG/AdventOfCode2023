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

let readFile fileName =
    try
        File.ReadLines fileName
        |>> String.split [|" "|]
        |>> Seq.map Int32.Parse
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let differences (row: int seq) =
    row
    |> Seq.pairwise
    |>> fun (a,b) -> a - b

let rec predict row =
    let diffs = differences row
    if Seq.exists ((<>) 0) diffs then
        Seq.head row + predict diffs
    else
        Seq.head row

let part1 (input: int seq seq) =
    input
    |>> Seq.rev
    |>> predict 
    |> Seq.sum
    |> sprintf "%i"

let part2 input = 
    input
    |>> predict 
    |> Seq.sum
    |> sprintf "%i"

module Tests =
    let private tests = 
        [
            //fun () -> Error "todo"
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
