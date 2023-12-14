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

let tiltNorth (input: char array2d) =
    let length = Array2D.length1 input 
    seq{0..(length - 1)}
    |> Seq.iter (fun row ->
        let mutable col = input[*,row] |> String.ofArray
        while String.isSubString ".O" col do
            col <- Regex.Replace(col,"(\.+)(O+)","$2$1",RegexOptions.RightToLeft)
        col
        |> Seq.iteri (fun i c -> input[i, row] <- c)
    )
    input

let tiltSouth (input: char array2d) =
    let length = Array2D.length1 input 
    seq{0..(length - 1)}
    |> Seq.iter (fun row ->
        let mutable col = input[*,row] |> String.ofArray
        while String.isSubString "O." col do
            col <- Regex.Replace(col,"(O+)(\.+)","$2$1")
        col
        |> Seq.iteri (fun i c -> input[i, row] <- c)
    )
    input

let tiltWest (input: char array2d) =
    let length = Array2D.length2 input 
    seq{0..(length - 1)}
    |> Seq.iter (fun row ->
        let mutable col = input[row,*] |> String.ofArray
        while String.isSubString ".O" col do
            col <- Regex.Replace(col,"(\.+)(O+)","$2$1",RegexOptions.RightToLeft)
        col
        |> Seq.iteri (fun i c -> input[row,i] <- c)
    )
    input

let tiltEast (input: char array2d) =
    let length = Array2D.length2 input 
    seq{0..(length - 1)}
    |> Seq.iter (fun row ->
        let mutable col = input[row,*] |> String.ofArray
        while String.isSubString "O." col do
            col <- Regex.Replace(col,"(O+)(\.+)","$2$1")
        col
        |> Seq.iteri (fun i c -> input[row,i] <- c)
    )
    input

let spinCycle (input: char array2d) =
    input
    |> tiltNorth
    |> tiltWest
    |> tiltSouth
    |> tiltEast

let load  (input: char array2d) =
    let height = Array2D.length1 input
    input
    |> Array2D.mapi (fun col row c -> if c = 'O' then height - col else 0)
    |> Seq.cast<int>
    |> Seq.sum
let part1 (input: char array2d) =
    input
    |> Array2D.copy
    |> tiltNorth
    |> load
    |> sprintf "%i"

let part2 (input: char array2d) = 
    seq{1..1000}
    |> Seq.fold 
        (fun acc i ->
            let newMap = spinCycle acc
            Console.WriteLine $"{i} cycles, load {load newMap}"
            newMap
        )
        input
    |> load
    |> sprintf "%i"

    //TODO: Programatically find sequence lenght (14), calculate modulus (0) and pick correct value (the last one)

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
