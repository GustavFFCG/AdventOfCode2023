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

let readFile fileName =
    try
        File.ReadLines fileName
        |> array2D
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let part1 (input: char array2d) =
    let height = Array2D.length1 input 
    seq{0..(height - 1)}
    |>> (fun row ->
        let mutable col = input[*,row] |> String.ofArray
        while String.isSubString ".O" col do
            col <- Regex.Replace(col,"(\.+)(O+)","$2$1",RegexOptions.RightToLeft)
        //Console.WriteLine $"changed {col} to {newCol}"
        col
    )
    |> Seq.sumBy ( fun col ->
        col
        |> Seq.mapi (fun i c -> if c = 'O' then height - i else 0)
        |> Seq.sum
        )
    |> sprintf "%i"

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
