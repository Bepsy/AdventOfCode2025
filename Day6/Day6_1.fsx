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
let numberParser =  many (spaces >>. pint64 .>> spaces)

let numbers = input[0..input.Length - 2] |> Array.map (fun line -> runAndUnwrap numberParser line) |> Array.toList |> List.transpose

// parse operators
let inputOperators = input[input.Length-1]
let operatorParser = many (spaces >>. pstring "+" <|> pstring "*" .>> spaces)
let operators  = runAndUnwrap operatorParser inputOperators

let calculate operator (column:list<int64>) =   
    match operator with 
    | "+" -> column |> List.fold (fun acc elem -> acc + elem) 0L
    | "*" -> column |> List.fold (fun acc elem -> acc * elem) 1L
    | _ -> failwith "no valid operator"

let result = operators |> List.mapi (fun index operator -> calculate operator numbers[index]) |> List.sum