#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus


type Card =
    { CardNo: int
      Winning: Set<int>
      Mine: Set<int> }

let fileName =
    fsi.CommandLineArgs
    |> List.ofArray
    |> function
        | _ :: s :: _ -> Some s
        | _ -> None

let parseInput (str: string) =
    let idstr, winningstr, mineStr =
        String.split ([| ": "; " | " |]) str
        |> List.ofSeq
        |> function
            | [ s1; s2; s3 ] -> s1, s2, s3
            | other -> failwith "other"

    { CardNo = idstr.Substring(4) |> Int32.Parse
      Winning =
        winningstr
        |> String.split ([| "  "; " " |])
        |> Seq.where (fun s -> s <> "")
        |>> Int32.Parse
        |> Set.ofSeq
      Mine =
        mineStr
        |> String.split ([| "  "; " " |])
        |> Seq.where (fun s -> s <> "")
        |>> Int32.Parse
        |> Set.ofSeq }

let readFile fileName =
    try
        File.ReadLines fileName |>> parseInput |> Ok
    with
    | ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}"

let part1 (input: Card seq) =
    input
    |> Seq.sumBy (fun c ->
        Set.intersect c.Winning c.Mine
        |> Set.count
        |> function
            | 0 -> 0
            | i -> pown 2 (i - 1))
    |> sprintf "%i"

let part2 (input: Card seq) =
    let cardScore c  =
        Set.intersect c.Winning c.Mine |> Set.count

    input
    |> Seq.rev
    |> Seq.fold
        (fun acc c ->
            cardScore c
            |> function
                | 0 -> acc |> Map.add c.CardNo 1
                | n ->
                    let value =
                        seq { c.CardNo + 1 .. c.CardNo + n }
                        |> Seq.where (fun x -> Map.containsKey x acc )
                        |> Seq.sumBy (fun x -> acc[x])
                    acc |> Map.add c.CardNo (1 + value)

        )
        Map.empty
    |> Map.values
    |> Seq.sum
    |> sprintf "%i"

module Tests =
    let private tests =
        [
        //fun () -> Error "todo"
        ]

    let run () =
        tests
        |> List.fold (fun state test -> state |> Result.bind test) (Ok())
        |>> fun () -> "All tests Ok"

let input = fileName |>> readFile

match input with
| Some input ->
    Result.map3
        (sprintf "Successful run!\r\nTests: %s\r\nPart1: %s\r\nPart2: %s")
        (Tests.run ())
        (input |>> part1)
        (input |>> part2)
| None -> Tests.run () |>> sprintf "Tests only:\r\n %s"
|> function
    | Ok s -> s
    | Error s -> sprintf "Error: %s" s
|> Console.WriteLine
