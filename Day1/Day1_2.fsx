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

let directionParser = (stringReturn "R"  Right) <|> (stringReturn "L" Left) .>>. pint32 .>> spaces

let parsedInput = parse (many directionParser) input |> List.map (fun (dir, step) -> {direction = dir; distance = step }) 

let rotate position {direction = direction; distance = distance}  =
    let rawPosition = 
        match direction with
        | Right -> (position + distance)%100
        | Left -> (position - distance)%100
    if rawPosition < 0 then rawPosition + 100 else rawPosition

let getClicks position {direction = direction; distance = distance} =
    let initClicks = distance/100
    let additionClicks = 
        match direction with
        | Right -> if position + distance%100 >= 100 then 1 else 0
        | Left -> if position <> 0 && position - distance%100 <= 0 then 1 else 0
    initClicks + additionClicks

let rotateWithClicks (clicks, current) rotation =
    let addedClicks = getClicks current rotation
    let newPosition = rotate current rotation
    (clicks + addedClicks, newPosition)

let clicks = List.scan rotateWithClicks (0,50) parsedInput |> List.last |> fun (clicks, position) -> clicks

printf "%A" clicks
