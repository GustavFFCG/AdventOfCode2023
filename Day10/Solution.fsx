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

let step dir (pos: int*int) =
    match dir with
        | Up -> (fst pos, snd pos - 1)
        | Down -> (fst pos, snd pos + 1)
        | Left -> (fst pos - 1, snd pos)
        | Right -> (fst pos + 1, snd pos)

let part1 (input: Map<(int*int),char>) =
    let canGo direction pos : (int*int) option =
        let maybeStep = step direction pos
        Map.tryFind maybeStep input
        |> Option.map (fun c ->
            match direction  with
            | Up ->
                Seq.contains input[pos] [|'S';'|';'J';'L'|] &&
                Seq.contains c [|'S';'|';'7';'F'|]
            | Down ->
                Seq.contains input[pos] [|'S';'|';'7';'F'|] &&
                Seq.contains c [|'S';'|';'J';'L'|]
            | Left ->
                Seq.contains input[pos] [|'S';'-';'J';'7'|] &&
                Seq.contains c [|'S';'-';'L';'F'|]
            | Right ->
                Seq.contains input[pos] [|'S';'-';'L';'F'|] &&
                Seq.contains c [|'S';'-';'J';'7'|])

        |>> fun x -> (x, maybeStep)
        >>= Option.ofPair

    let possibleSteps pos =
        [ Up;Down;Left;Right]
        |> List.choose (fun d -> canGo d pos)
        
    let startPos = input |> Map.findKey (fun _ c -> c = 'S' )

    let rec createpath pos prevPos path =
        if Set.contains pos path then Some path
        else
            match prevPos with
            | Some prevPos ->
                possibleSteps pos
                |> List.where (fun p -> p <> prevPos)
                |> function
                | [] -> None
                | head::tail -> createpath head (Some pos) (Set.add pos path)
            | None ->
                possibleSteps pos
                |> List.pick (fun next -> createpath next (Some pos) (Set.add pos path))
                |> Some
            
    createpath startPos None Set.empty
    |>> Set.count
    |>> fun count -> count / 2
    |>> sprintf "%i"
    |> Option.defaultValue "No paths found" 

let part2 input = 
    let canGo direction pos : (int*int) option =
        let maybeStep = step direction pos
        Map.tryFind maybeStep input
        |> Option.map (fun c ->
            match direction  with
            | Up ->
                Seq.contains input[pos] [|'S';'|';'J';'L'|] &&
                Seq.contains c [|'S';'|';'7';'F'|]
            | Down ->
                Seq.contains input[pos] [|'S';'|';'7';'F'|] &&
                Seq.contains c [|'S';'|';'J';'L'|]
            | Left ->
                Seq.contains input[pos] [|'S';'-';'J';'7'|] &&
                Seq.contains c [|'S';'-';'L';'F'|]
            | Right ->
                Seq.contains input[pos] [|'S';'-';'L';'F'|] &&
                Seq.contains c [|'S';'-';'J';'7'|])

        |>> fun x -> (x, maybeStep)
        >>= Option.ofPair

    let possibleSteps pos =
        [ Up;Down;Left;Right]
        |> List.choose (fun d -> canGo d pos)
        
    let startPos = input |> Map.findKey (fun _ c -> c = 'S' )

    let rec createpath pos prevPos path =
        if List.contains pos path then Some path
        else
            match prevPos with
            | Some prevPos ->
                possibleSteps pos
                |> List.where (fun p -> p <> prevPos)
                |> function
                | [] -> None
                | head::tail -> createpath head (Some pos) (pos::path)
            | None ->
                possibleSteps pos
                |> List.pick (fun next -> createpath next (Some pos) (pos::path))
                |> Some
            
    createpath startPos None []
    |> function
    | Some path ->
        path
        |> List.mapi (fun i (x,y) ->
            let next = (i + 1) % List.length path
            x * (snd path[next]) - (fst path[next]) * y
        )
        |> List.sum
        |> abs
        |> fun i -> (i - List.length path) /2 + 1
        |> sprintf "%i"
    | None -> "Error! no path found"



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
