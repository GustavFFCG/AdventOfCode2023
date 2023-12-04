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

type Coord = {x:int; y:int}
type Input = {
    Symbols: Map<Coord, char>
    Values: Map<Coord, int>
}
let parseInput rowNo (row: string) =
    row
    |> Seq.fold 
        (
            fun (accInput, pendingValue, col) c ->
                match c with
                | '.' ->
                    pendingValue |> function
                        | None -> accInput, None, col + 1
                        | Some (s, x) ->
                            { accInput with 
                                Values = 
                                    accInput.Values
                                    |> Map.add 
                                        {x = x; y = rowNo}
                                        (Int32.Parse(s))
                            }, None, col + 1
                | c when Char.IsNumber c ->
                    pendingValue |> function
                        | None -> accInput, Some <| ((string c), col), col + 1
                        | Some (s, x) -> 
                            accInput, 
                            Some <| ($"%s{s}%c{c}", x), col + 1
                | c ->
                    let symbols = 
                        accInput.Symbols 
                        |> Map.add
                            {x = col; y = rowNo}
                            c
                    pendingValue |> function
                        | None -> { accInput with Symbols = symbols} , None, col + 1
                        | Some (s, x) ->
                            { 
                                Symbols = symbols
                                Values = 
                                    accInput.Values
                                    |> Map.add 
                                        {x = x; y = rowNo}
                                        (Int32.Parse(s))
                            }, None, col + 1
        )
        ({
            Symbols = Map.empty
            Values = Map.empty
        }, None, 0)
    |> function
        | input, None, _ -> input
        | input, Some (s, x), _ ->
            { input with 
                Values = 
                    input.Values
                    |> Map.add 
                        {x = x; y = rowNo}
                        (Int32.Parse(s))
            }


    
let readFile fileName =
    try
        File.ReadLines fileName
        |> Seq.mapi parseInput
        |> Seq.fold 
            (fun acc x -> 
                {Symbols = Map.union acc.Symbols x.Symbols; Values = Map.union acc.Values x.Values }
            )
            { Symbols = Map.empty;Values = Map.empty }

        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let hasAdjoiningSymbol (coord: Coord) (intValue: int) (symbols: Set<Coord>) =
    let adjoiningCoords =   
        let startX = coord.x
        let stopX = coord.x + (intValue.ToString() |> String.length) - 1
        let xSeq = seq {startX - 1 .. stopX + 1}
        Set.empty
        |> Set.add {coord with x = startX - 1}
        |> Set.add {coord with x = stopX + 1}
        |> Set.union ( xSeq |>> (fun x -> {x = x; y = coord.y - 1 }) |> Set.ofSeq )
        |> Set.union ( xSeq |>> (fun x -> {x = x; y = coord.y + 1 }) |> Set.ofSeq )
    symbols
    |> Set.intersect adjoiningCoords
    |> Set.isEmpty
    |> not

let isAdjoiningSymbol (coord: Coord) (intValue: int) (symbol: Coord) =
    let startX = coord.x
    let stopX = coord.x + (intValue.ToString() |> String.length) - 1
    let xSeq = seq {startX - 1 .. stopX + 1}
    Set.empty
    |> Set.add {coord with x = startX - 1}
    |> Set.add {coord with x = stopX + 1}
    |> Set.union ( xSeq |>> (fun x -> {x = x; y = coord.y - 1 }) |> Set.ofSeq )
    |> Set.union ( xSeq |>> (fun x -> {x = x; y = coord.y + 1 }) |> Set.ofSeq )
    |> Set.contains symbol 

let part1 (input: Input) =
    input
    |> fun input -> input.Values
    |> Map.filter (fun c v -> input.Symbols |> Map.keys |> Set.ofSeq |> hasAdjoiningSymbol c v )
    |> Map.values
    |> Seq.sum
    |> sprintf "%i"

let part2 (input: Input) = 
    let gearSymbols = 
        input.Symbols |> Map.filter (fun _ c -> c = '*')
        |> Map.keys

    gearSymbols
    |>> (fun symbolCoord ->
        input.Values
        |> Map.filter (fun c v -> isAdjoiningSymbol c v symbolCoord)
        |> Map.values
        |> List.ofSeq
    )
    |> Seq.choose (fun x -> if List.length x = 2 then Some (x[0], x[1]) else None )
    |> Seq.sumBy (fun (a, b) -> a * b )
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
