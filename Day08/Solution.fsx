#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus
open System.Text.RegularExpressions

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function 
    | _::s::_ -> Some s 
    | _ -> None

type Input = {
    Directions : char list
    Nodes: Map<string, (string*string)>
}
let parseInput (input:string seq) =
    {
        Directions = input |> Seq.head |> List.ofSeq
        Nodes =
            input
            |> Seq.tail
            |> Seq.skip 1
            |> Seq.map (fun s ->
                let m = Regex.Match( s,"([\dA-Z]{3}) = \(([\dA-Z]{3}), ([\dA-Z]{3})\)")
                m.Groups[1].Value, (m.Groups[2].Value, m.Groups[3].Value))
            |> Map.ofSeq
    }

let readFile fileName =
    try
        File.ReadLines fileName
        |> parseInput
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let part1 (input:Input) =
    let seqLength = input.Directions |> Seq.length
    let rec traverse stepcount key =
        if key = "ZZZ" then stepcount
        else
            input.Directions[stepcount % seqLength ]
            |> function
            | 'L' -> traverse (stepcount + 1) (fst input.Nodes[key])
            | 'R' -> traverse (stepcount + 1) (snd input.Nodes[key])
            | _ -> failwith "illegal direction"
    traverse 0 "AAA"
    |> sprintf "%i"

let part2 input =
    let lcm (ints:int64 list) =
        let temp = ints |> Array.ofSeq
        while Array.distinct temp |> Array.length <> 1 do
            let i = Array.findIndex (fun i -> i = Array.min temp) temp
            temp[i] <- temp[i] + ints[i]
        temp[0]
                
    let seqLength = input.Directions |> Seq.length
    let rec traverse stepcount (key:string) =
        if key[2] = 'Z' then stepcount
        else
            input.Directions[stepcount % seqLength ]
            |> function
            | 'L' -> traverse (stepcount + 1) (fst input.Nodes[key])
            | 'R' -> traverse (stepcount + 1) (snd input.Nodes[key])
            | _ -> failwith "illegal direction"
    let keys =
        input.Nodes.Keys
        |> Seq.where (fun s -> s[2] = 'A')

    let pathLengths =
        keys
        |>> traverse 0

    pathLengths
    |> List.ofSeq
    |>> int64
    |> lcm
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
