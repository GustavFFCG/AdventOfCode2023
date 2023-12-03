#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus

type Color = 
| Blue
| Green
| Red

type Game = {
    GameId: int
    Draws: Map<Color, int> seq
}

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function 
    | _::s::_ -> Some s 
    | _ -> None


let parseGame (row: string) =
    let gameId, drawData =
        String.split [|": "|] row |> List.ofSeq
        |> fun l -> 
            l[0] |> fun s -> s.Substring(5) |> Int32.Parse
            , l[1] |> String.split [|"; "|] |>> String.split [|", "|]
    let draws =
        drawData
        |>> Seq.map (fun s ->
            s |> String.split [|" "|] |> List.ofSeq
            |> function
            | [i;c] -> 
                c |> function 
                    | "blue" -> Blue 
                    | "red" -> Red 
                    | "green" -> Green 
                    | other -> failwith $"unexpected color {other}"
                , Int32.Parse i
            | other -> failwith $"unexpected draw {other}" 
        ) 
        |>> Map.ofSeq
    {
        GameId = gameId
        Draws = draws
    }

let readFile fileName =
    try
        File.ReadLines fileName
        |>> parseGame
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let part1 (input: Game seq) =
    let colorIsPossible existing color (draw: Map<Color, int>) =
        let cubesInDraw =
            draw
            |> Map.tryFind color
            |> Option.defaultValue 0
        cubesInDraw <= existing
        
    let drawIsPossible (draw: Map<Color, int>) =
        colorIsPossible 12 Red draw
        && colorIsPossible 13 Green draw
        && colorIsPossible 14 Blue draw
    
    input
    |> Seq.where (fun g -> 
        g.Draws |> Seq.forall (drawIsPossible) )
    |> Seq.sumBy (fun g -> g.GameId)
    |> sprintf "%i"

let part2 (input: Game seq) =
    let minimalColor color (draws: Map<Color, int> seq) =
        draws
        |>> fun d -> d |> Map.tryFind color |> Option.defaultValue 0
        |> Seq.max
    let minimalBag (draws: Map<Color, int> seq) =
        (minimalColor Red draws) * (minimalColor Green draws) * (minimalColor Blue draws)

    input
    |>> fun g -> g.Draws
    |>> minimalBag
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
