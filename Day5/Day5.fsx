open System
open System.Collections.Generic
open System.IO

let inputPath = "input.txt"
//let inputPath = "inputExample.txt"

let input = File.ReadAllText inputPath

let ranges = (input.Split "\n\n")[0]
            |> fun s -> s.Split [|',';'\r'; '\n'|] 
            |> Array.filter (fun s -> s <> "") 
            |> Array.map (fun s -> (s.Split [|'-'|]))
            |> Array.map (fun arr -> int64 arr[0], int64 arr[1])
            |> List.ofArray
            |> List.sortBy (fun (k,v) -> k)

let ids = (input.Split "\n\n")[1]
            |> fun s -> s.Split [|'\r'; '\n'|]
            |> Array.map (fun s -> int64 s)
            |> Array.toList
            |> List.sort

let removeOverlapping (ranges:(int64 * int64) list) = 
    let isOverlap (_,end1) (start2,end2) = end1 >= start2 && end1 < end2 
    let isWithin (_,end1) (start2,end2) = end1 >= start2 && end1 >= end2
    let combine (start1,_) (_,end2) = (start1, end2)
    let rec loop result ranges = 
        match ranges with
        | [] -> result
        | a::[]-> (a::result)
        | a::b::rest when isOverlap a b-> loop result (combine a b::rest)
        | a::b::rest when isWithin a b -> loop result (a::rest)
        | a::b::rest -> loop (a::result) (b::rest)
    loop [] ranges

let startRange (startRange,_) = startRange
let endRange (_,endRange) = endRange

let checkIds (ranges:list<int64*int64>) (ids:list<int64>) =
    let rec loop result indexRange ids =
        match ids with 
        | [] | [_] -> result
        | _ when indexRange = ranges.Length -> result
        | currentId::rest when currentId < startRange ranges[indexRange] -> loop result indexRange rest
        | currentId::rest when currentId > endRange ranges[indexRange]-> loop result (indexRange+1) ids
        | currentId::rest -> loop (currentId::result) indexRange rest
    loop [] 0 ids

let rangesNoOverlap = removeOverlapping ranges |> List.rev
let result1 = checkIds rangesNoOverlap ids |> List.length
let result2 = rangesNoOverlap |> List.fold (fun acc range -> acc + endRange range - startRange range + 1L) 0L