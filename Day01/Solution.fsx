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
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let part1 (input: string seq) =
    input
    |>> fun s -> s |> Seq.where (fun (c:char) -> Char.IsNumber c )
    |>> fun s -> $"%c{Seq.head s}%c{Seq.last s}"
    |> Seq.sumBy Int32.Parse
    |> sprintf "%i"


let matcher i strRep str = 
    let expression = $"^(%i{i}|%s{strRep})(.*)$"
    let m = Regex.Match(str, expression)
    if m.Success then Some (i, m.Groups[2].Value) else None

let (|Zero|_|)  = matcher 0 "zero"
let (|One|_|)   = matcher 1 "one"
let (|Two|_|)   = matcher 2 "two"
let (|Three|_|) = matcher 3 "three"
let (|Four|_|)  = matcher 4 "four"
let (|Five|_|)  = matcher 5 "five"
let (|Six|_|)   = matcher 6 "six"
let (|Seven|_|) = matcher 7 "seven"
let (|Eight|_|) = matcher 8 "eight"
let (|Nine|_|)  = matcher 9 "nine"

let part2 (input: string seq) = 
    let rec parser acc (s: string) =
        match s with
        | Zero (i, tail) 
        | One (i, tail) 
        | Two (i, tail) 
        | Three (i, tail) 
        | Four (i, tail) 
        | Five (i, tail) 
        | Six (i, tail) 
        | Seven (i, tail) 
        | Eight (i, tail) 
        | Nine (i, tail) 
            -> if tail = "" then i::acc else parser (i::acc) (s.Substring(1))
        | "" -> acc
        | s -> parser acc (s.Substring(1))

    input
    |>> parser []
    |> Seq.sumBy (fun l -> List.head l + 10 * List.last l )
    |> sprintf "%i"

module Tests =
    let private tests = 
        [
            fun () -> 
                [|
                    "two1nine"
                    "eightwothree"
                    "abcone2threexyz"
                    "xtwone3four"
                    "4nineeightseven2"
                    "zoneight234"
                    "7pqrstsixteen"
                |]
                |> part2
                |> function "281" -> Ok () | other -> Error other
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
