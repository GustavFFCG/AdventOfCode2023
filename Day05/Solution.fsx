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

type Mapping = {
    DestStart: Int64
    SourceStart: Int64
    Range: Int64
}
type Input = {
    Seeds: Int64 seq
    SeedToSoil: Mapping seq
    SoilToFertilizer: Mapping seq
    FertilizserToWater: Mapping seq
    WaterToLight: Mapping seq
    LightToTemperature: Mapping seq
    TemperatureToHumidity: Mapping seq
    HumidityToLocation: Mapping seq
}

let ParseInput (input:string seq) =
    let seeds = 
        input
        |> Seq.head
        |> String.split [|" "|]
        |> Seq.tail
        |>> Int64.Parse

    let mappings =
        input
        |> Seq.split [| [|""|]|]
        |> Seq.tail
        |>> Seq.tail //remove header
        |> List.ofSeq

    let parseMappings (mapping: string seq) = 
        mapping
        |>> fun s -> s |> String.split [|" "|] |>> Int64.Parse |> List.ofSeq
        |>> function
        | [destStart;sourceStart;range] ->
            {
                DestStart = destStart
                SourceStart = sourceStart
                Range = range
            }
        | other -> failwith $"unexpected input {other}"

    {
        Seeds = seeds
        SeedToSoil = parseMappings mappings[0]
        SoilToFertilizer = parseMappings mappings[1]
        FertilizserToWater = parseMappings mappings[2]
        WaterToLight = parseMappings mappings[3]
        LightToTemperature = parseMappings mappings[4]
        TemperatureToHumidity = parseMappings mappings[5]
        HumidityToLocation = parseMappings mappings[6]
    }
let readFile fileName =
    try
        File.ReadLines fileName
        |> ParseInput
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let part1 input =
    let mapBy (mappings: Mapping seq) source =
        mappings
        |> Seq.tryFind (fun m -> 
            source >= m.SourceStart
            && source < m.SourceStart + m.Range)
        |>> (fun m -> m.DestStart + source - m.SourceStart)
        |> Option.defaultValue source

    input.Seeds
    |>> mapBy input.SeedToSoil
    |>> mapBy input.SoilToFertilizer
    |>> mapBy input.FertilizserToWater
    |>> mapBy input.WaterToLight
    |>> mapBy input.LightToTemperature
    |>> mapBy input.TemperatureToHumidity
    |>> mapBy input.HumidityToLocation
    |> Seq.min
    |> sprintf "%i"

let part2 (input: Input) =
    let mapRanges (mappings: Mapping seq) (inputRange: (int64*int64)) : (int64*int64) seq =
        let relevantMappings = 
            mappings
            |> Seq.filter (fun m -> (snd inputRange) <= m.SourceStart  && (fst inputRange) < m.SourceStart + m.Range)
            |> Seq.sortBy (fun m -> m.SourceStart)
        
        relevantMappings
        |> Seq.fold 
            ( fun (acc, rangeLeft) m -> 
                rangeLeft
                |> function
                    | Some (start, stop) ->
                        let beforeRange =
                            if start > m.SourceStart then Some (start, m.SourceStart - 1L) else None
                        let midRange =
                            (Math.Max (start, m.SourceStart), Math.Min (stop, m.SourceStart + m.Range - 1L))
                        let afterRange =
                            if stop <= m.SourceStart + m.Range then Some (m.SourceStart + m.Range) else None
                        beforeRange |> function
                        | Some r -> ([r;midRange], rangeLeft)
                        | None -> ([midRange], rangeLeft)
                    | None -> (acc, None)
            )
            ([ inputRange ], Some inputRange)
        |> function | (acc, Some r) -> r::acc | (acc, None) -> acc
    input.Seeds
    |> Seq.chunkBySize 2
    |>> List.ofArray
    |>> (function | [a; b] -> (a, b) | _ -> failwith "not even number")
    >== (mapRanges input.SeedToSoil)
    >== (mapRanges input.SoilToFertilizer)
    >== (mapRanges input.FertilizserToWater)
    >== (mapRanges input.WaterToLight)
    >== (mapRanges input.LightToTemperature)
    >== (mapRanges input.TemperatureToHumidity)
    >== (mapRanges input.HumidityToLocation)
    |>> fst
    |>> Seq.min
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
