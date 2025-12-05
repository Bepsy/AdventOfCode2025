open System
open System.IO

// let inputPath = "inputExample.txt"
let inputPath = "Input.txt"

let input = File.ReadAllText inputPath

let inputParsed = input.Split [|',';'\r'; '\n'|] 
                |> Array.filter (fun s -> s <> "") 
                |> Array.map (fun s -> (s.Split [|'-'|]))
                |> Array.map (fun arr -> (float (Int64.Parse arr[0]), float (Int64.Parse arr[1])))  

let checkSequence sequence =
    let length = (Math.Log10 sequence |> Math.Floor) + 1.0
    let first = Math.Floor (sequence / (10.0**(length/2.0)))
    let last = sequence - first * 10.0 **(length/2.0)
    first = last

let rec LoopRange current endRange (result:List<float>) =
    let LoopNextRange = LoopRange (current+1.0) endRange
    match current with
    | _ when current > endRange -> result |> List.sum
    | _ when (checkSequence current) -> LoopNextRange (current :: result) 
    | _ -> LoopNextRange result

let LoopRangeForLoop (current, endRange) = LoopRange current endRange []

let result = inputParsed |> Array.map LoopRangeForLoop |> Array.sum

printf "%A" (result |> int64)