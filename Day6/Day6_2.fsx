open System
open System.Collections.Generic
open System.IO
#r "nuget: Fparsec"
open FParsec

let inputPath = "input.txt"
//let inputPath = "inputExample.txt"
let input = File.ReadAllLines inputPath

let runAndUnwrap parser input=
    let result = run parser input
    match result with
    | Success (value, _, _) -> value
    | Failure (err, _, _) -> failwith err

// parse numbers 
let inputNumbersTransposed = input[0..input.Length - 2]  |> Seq.transpose |> Seq.map (fun column -> new string (Seq.toArray column)) |> Seq.toList

let groupNumbers (numbers:list<string>) =
    let rec loop (result:list<list<string>>) input =
        match input with 
        | [] -> result
        | x::rest when x = "    " -> loop ([]::result) rest
        | x::rest -> loop ((x::result[0])::result[1..]) rest
    loop [[]] numbers |> List.rev

let numberParser = spaces >>. pint64 .>> spaces

let numbers = groupNumbers inputNumbersTransposed |> List.map (fun group -> group |> List.map (runAndUnwrap numberParser))

// parse operators
let inputOperators = input[input.Length-1]
let operatorParser = many (spaces >>. pstring "+" <|> pstring "*" .>> spaces)
let operators  = runAndUnwrap operatorParser inputOperators

// calculate
let calculate operator (numberGroup:list<int64>) =   
    match operator with 
    | "+" -> numberGroup |> List.fold (fun acc elem -> acc + elem) 0L
    | "*" -> numberGroup |> List.fold (fun acc elem -> acc * elem) 1L
    | _ -> failwith "no valid operator"

let result = operators |> List.mapi (fun index operator -> calculate operator numbers[index]) |> List.sum