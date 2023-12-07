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
    Time: int64
    Distance: int64
}

let parseInput (input:string seq) =
    input
    |>> (String.split [|":"|] >> Seq.tail >> Seq.head)
    |>> String.split [|" "|]
    |> Seq.map (Seq.where (fun s -> s <> "") >> Seq.map (fun s -> (Int64.Parse(s))))
    |> List.ofSeq
    |> function
        | [timeRow;distRow] ->
            let distList = distRow |> List.ofSeq
            timeRow
            |> Seq.mapi (fun i t -> {Time = t; Distance = distList[i]})
        | _ -> failwith "unexpected input"

let parseInput2 (input:string seq) =
    input
    |>> (String.split [|":"|] >> Seq.tail >> Seq.head)
    |>> (String.replace " " "")
    |>> Int64.Parse
    |> List.ofSeq
    |> function
        | [time;dist] -> {Time = time; Distance = dist}
        | _ -> failwith "unexpected input"

let readFile fileName =
    try
        File.ReadLines fileName
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let distances time: Int64 seq =
    seq {1L..time - 1L}
    |>> fun chargeTime -> chargeTime * (int64 time - chargeTime)

let part1 (input: string seq) =
    input
    |> parseInput
    |>> (fun race ->
        distances race.Time
        |> Seq.where ((<) race.Distance)
    )
    |>> (Seq.length >> int64)
    |> Seq.fold (*) 1L
    |> sprintf "%i"

let part2 (input: string seq) =
    let roots (x:Input) =
        let sqrt = 
            Math.Sqrt ((float (x.Time * x.Time)) / 4.0 - (float x.Distance))
            |> int64
        (x.Time / 2L - sqrt), (x.Time / 2L + sqrt)
    input
    |> parseInput2
    |> roots
    |> fun (a, b) -> b - a + 1L
    |> sprintf "%i"

module Tests =
    let private tests = 
        [
            fun () -> 
                distances 7 |> List.ofSeq |> List.sort
                |> function
                | [6L;6L;10L;10L;12L;12L] -> Ok ()
                | other -> Error $"{other}"
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
