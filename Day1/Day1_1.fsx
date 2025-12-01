#r "nuget: FParsec"
open FParsec

open System.IO
open Microsoft.FSharp.Collections

let input = File.ReadAllText "input.txt" 

type Direction = 
    | Right
    | Left

type Rotation = { 
    direction : Direction
    distance : int
}

let parse p str =
    match run p str with
    | Success(result, _, _) -> result
    | Failure(msg,_,_) -> failwith $"Parse error: {msg}"

let  directionParser = (stringReturn "R"  Right) <|> (stringReturn "L" Left) .>>. pint32 .>> spaces

let parsedInput = parse (many directionParser) input |> List.map (fun (dir, step) -> {direction = dir; distance = step }) 

let rotate current {direction = direction; distance = distance}  =
    let rawCurrent = 
        match direction with
        | Right -> (current + distance)%100
        | Left -> (current - distance)%100
    if rawCurrent < 0 then rawCurrent + 100 else rawCurrent


let outcome = List.scan rotate 50 parsedInput |> List.filter (fun a -> a = 0) |> List.length  

printf "%A" outcome