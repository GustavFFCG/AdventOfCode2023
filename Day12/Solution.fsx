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
    Data:string
    Groups: int list
}
let parseInput s =
    s
    |> String.split [|" "|]
    |> List.ofSeq
    |> function
        | [data;groups] ->
            {
                Data = data
                Groups =
                    groups
                    |> String.split [|","|]
                    |>> Int32.Parse
                    |> List.ofSeq
            }
        | other -> failwith "unexpected input"

let readFile fileName =
    try
        File.ReadLines fileName
        |>> parseInput
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let isSolution (groups: int list) string =
    string
    |> String.split [|"."|]
    |> Seq.where ((<>) "")
    |>> Seq.length
    |> List.ofSeq
    |> fun x -> x = groups

let part1 (input: Input seq) =
    let replaceAt i c (s: string) = 
        let arr = s.ToCharArray()
        arr.[i] <- c
        String arr
    input
    |> Seq.mapi (fun i row ->
        let count = row.Data |> Seq.where ((=) '?') |> Seq.length
        let unsurePos =
            row.Data
            |> Seq.choosei (fun i c -> if c = '?' then Some i else None)
        let startingRow = String.replace "?" "#" row.Data
        unsurePos
        |> Seq.fold 
            (fun (acc: string seq) i ->
                acc
                |> Seq.bind (fun s -> [|s;( s |> replaceAt i '.')|])
            )
            [|startingRow|]
        |> Seq.where (fun s -> isSolution row.Groups s)
        |> Seq.length
    )
    |> Seq.sum
    |> sprintf "%i"

let part2 input = 
    let replaceAt i c (s: string) = 
        let arr = s.ToCharArray()
        arr.[i] <- c
        String arr
    input
    |>> fun row ->
        {
            Data = 
                seq{1..5} 
                |> Seq.map (fun _ -> row.Data)
                |> fun x -> String.Join('?', x)
            Groups = 
                seq{1..5}
                |>> (fun _ -> row.Groups)
                |> List.concat
        } 
    |> Seq.mapi (fun i row ->
        Console.WriteLine $"Row {i}"
        let unsurePos =
            row.Data
            |> Seq.choosei (fun i c -> if c = '?' then Some i else None)
        let startingRow = String.replace "?" "#" row.Data
        unsurePos
        |> Seq.fold 
            (fun (acc: string seq) i ->
                acc
                |> Seq.bind (fun s -> [|s;( s |> replaceAt i '.')|])
            )
            [|startingRow|]
        |> Seq.where (fun s -> isSolution row.Groups s)
        |> Seq.length
    )
    |> Seq.sum
    |> sprintf "%i"

module Tests =
    let private tests = 
        [
            fun () -> isSolution [1;1;3] "#.#.###" |> function | true -> Ok () | false ->  Error "expected true"
            fun () -> isSolution [1;1;3] "..#..#....###." |> function | true -> Ok () | false ->  Error "expected true"
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
