open System
open System.Collections.Generic
open System.IO
#r "nuget: Fparsec"
open FParsec

let inputPath = "input.txt"
//let inputPath = "inputExample.txt"
let input = File.ReadAllLines inputPath

let inputNumbers = input[0..input.Length - 2]

let inputOperators = input[input.Length-1]

let unwrapResult =
    function
    | Success (value, _, _) -> value
    | Failure (err, _, _) -> failwith err

let parseLineOperator line = unwrapResult (run (many (spaces >>. pstring "+" <|> pstring "*" .>> spaces)) line)

let parseNumbers line = unwrapResult (run (many (spaces >>. pint64 .>> spaces)) line)

let numberMatrix = inputNumbers |> Array.toList |> List.map (fun line -> parseNumbers line) 

let operators = parseLineOperator inputOperators

let transposedNumberMatrix = numberMatrix |> List.transpose

let calculatePart1 operator (column:list<int64>) =   
    match operator with 
    | "+" -> column |> List.fold (fun acc elem -> acc + elem) 0L
    | "*" -> column |> List.fold (fun acc elem -> acc * elem) 1L
    | _ -> failwith "no valid operator"

let result = operators |> List.mapi (fun index operator -> calculatePart1 operator transposedNumberMatrix[index]) |> List.sum