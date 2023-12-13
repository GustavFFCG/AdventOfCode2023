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

type Reflection = 
    | Horizontal of int
    | Vertical of int
let pattern (rows: string seq) =
    array2D rows

let readFile fileName =
    try
        File.ReadLines fileName
        |> Seq.split [|[|""|]|]
        |>> pattern
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let isHorizontalReflectionBelow (pattern: char array2d) i =
    let height = Array2D.length1 pattern
    seq{0..(Math.Min(i,height - 2 - i ))}
    |> Seq.exists (fun row ->
        let s1 = pattern[i - row,*]
        let s2 = pattern[i + 1 + row, *]
        s1 <> s2
        )
    |> not

let isSmudgedHorizontalReflectionBelow (pattern: char array2d) i =
    let height = Array2D.length1 pattern
    seq{0..(Math.Min(i,height - 2 - i ))}
    |> Seq.sumBy (fun row ->
        let s1 = pattern[i - row,*]
        let s2 = pattern[i + 1 + row, *]
        Seq.map2 (<>) s1 s2
        |> Seq.where id
        |> Seq.length
        )
    |> ((=) 1)

let findHorizontalReflection (checker:(char array2d -> int -> bool)) (pattern: char array2d) =
    seq{0..(Array2D.length1 pattern - 2)}
    |> Seq.tryFind (checker pattern)
    |>> (+) 1

let isVerticalReflectionRightOf (pattern: char array2d) i =
    let width = Array2D.length2 pattern
    seq{0..(Math.Min(i,width - 2 - i ))}
    |> Seq.exists (fun col ->
        let s1 = pattern[*,i - col]
        let s2 = pattern[*,i + 1 + col]
        s1 <> s2
        )
    |> not

let isSmudgedVerticalReflectionRightOf (pattern: char array2d) i =
    let width = Array2D.length2 pattern
    seq{0..(Math.Min(i,width - 2 - i ))}
    |> Seq.sumBy (fun col ->
        let s1 = pattern[*,i - col]
        let s2 = pattern[*,i + 1 + col]
        Seq.map2 (<>) s1 s2
        |> Seq.where id
        |> Seq.length
        )
    |> ((=) 1)

let findVerticalReflection (checker:(char array2d -> int -> bool)) (pattern: char array2d) =
    seq{0..(Array2D.length2 pattern - 2)}
    |> Seq.tryFind (checker pattern)
    |>> (+) 1

let part1 (input: char array2d seq)  =
    input
    |>> (fun pattern -> 
        findHorizontalReflection isHorizontalReflectionBelow pattern
        |> function
        | Some r -> Horizontal r
        | None -> 
            findVerticalReflection isVerticalReflectionRightOf pattern |>> Vertical
            |> Option.defaultWith (fun () -> failwith "Neither")
    )
    |> Seq.sumBy (function | Vertical x -> x | Horizontal x -> 100 * x )
    |> sprintf "%i"

let part2 input = 
    input
    |>> (fun pattern -> 
        findHorizontalReflection isSmudgedHorizontalReflectionBelow pattern
        |> function
        | Some r -> Horizontal r
        | None -> 
            findVerticalReflection isSmudgedVerticalReflectionRightOf pattern |>> Vertical
            |> Option.defaultWith (fun () -> failwith "Neither")
    )
    |> Seq.sumBy (function | Vertical x -> x | Horizontal x -> 100 * x )
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
