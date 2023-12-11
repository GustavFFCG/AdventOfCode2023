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
        |> Seq.mapi (fun x c -> if c <> '.' then Some ((x, y), c) else None)
        |> Seq.choose id
    )
    |> Seq.concat
    |> Map.ofSeq

let readFile fileName =
    try
        File.ReadLines fileName
        |> parseInput
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

type Direction =
    | Up
    | Down
    | Left
    | Right

let part1 (input: Map<(int*int),char>) =
    let canGo direction pos =
        match direction with
        | Up ->
            Map.tryFind (fst pos, snd pos - 1) input
            |>> fun c ->
                Seq.contains input[pos] [|'|';'J';'L'|] &&
                Seq.contains c [|'|';'7';'F'|]
            |> Option.defaultValue false
        | Down ->
            Map.tryFind (fst pos, snd pos - 1) input
            |>> fun c ->
                Seq.contains input[pos] [|'|';'7';'F'|] &&
                Seq.contains c [|'|';'J';'L'|]
            |> Option.defaultValue false
        | Left ->
            Map.tryFind (fst pos, snd pos - 1) input
            |>> fun c ->
                Seq.contains input[pos] [|'-';'J';'7'|] &&
                Seq.contains c [|'-';'L';'F'|]
            |> Option.defaultValue false
        | Right ->
            Map.tryFind (fst pos, snd pos - 1) input
            |>> fun c ->
                Seq.contains input[pos] [|'-';'L';'F'|] &&
                Seq.contains c [|'-';'J';'7'|]
            |> Option.defaultValue false

    let startPos = input |> Map.findKey (fun _ c -> c = 'S' )
    "todo"
let part2 input = 
    "todo"

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
