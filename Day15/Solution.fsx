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
        |> Seq.head
        |> String.split [|","|]
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let hash s = 
    Seq.fold
        (fun acc c -> 
            ((acc + int c) * 17) % 256
        )
        0 s

let part1 (input: string seq) =
    input
    |> Seq.sumBy hash
    |> sprintf "%i"

type Instruction =
    | Remove of string
    | Replace of string * int

let part2 input =
    let boxes : ((string*int) seq) array =
        seq{0..255}
        |>> fun _ -> Seq.empty
        |> Array.ofSeq
    input
    |>> fun s -> 
        if String.contains '-' s 
        then s |> String.replace "-" "" |> Remove
        else 
            s|> String.split [|"="|] |> List.ofSeq
            |> function 
                | [label;value] -> (label, Int32.Parse value) |> Replace
                | other -> failwith "unexpected input"
    |> Seq.iter (fun instruction ->
        match instruction with
            | Remove label ->
                let boxNo = hash label
                boxes[boxNo] <- boxes[boxNo] |> Seq.where (fun (b,_i) -> b <> label )
            | Replace (label, value) ->
                let boxNo = hash label
                boxes[boxNo] <- 
                    boxes[boxNo] |> Seq.tryFindIndex (fun (b,_i) -> b = label )
                    |> function
                        | Some slot -> boxes[boxNo] |> Seq.mapi (fun i x -> if i = slot then (label,value) else x)
                        | None -> boxes[boxNo] |> Seq.append [|(label,value)|]
        )

    boxes
    |>> Seq.rev
    |> Seq.mapi ( fun i box ->
        box
        |> Seq.mapi (fun j (_,strength) -> (i + 1) * (j + 1) * strength)
        |> Seq.sum
    )
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
