open System
open System.Collections.Generic
open System.IO

let inputPath = "input.txt"
//let inputPath = "inputExample.txt"
let input = File.ReadAllLines inputPath

let inputParsed = input 
                |> Array.map (fun line -> 
                    line |> 
                    Seq.map(fun c -> 
                        if c = '^' || c = 'S' then 1L else 0L)
                    |> Seq.toList)
                |> Array.toList

let splitPathOptions pathOptions =
    let rec loop (result:list<int64>) (pathOptions:list<int64>) =
        match pathOptions, result with
        | [],_ -> result |> List.rev
        | p::0L::prest, r::rrest when p > 0 -> loop (p :: 0L :: p + r :: rrest) prest
        | t::trest,_ -> loop (t::result) trest
    loop [] pathOptions

let getNextPathOptions (pathOptions:list<int64>) (splitters:list<int64>) =
    let flip x = 1L - x
    let unchangedPathOptions = splitters |> List.map flip |> List.map2 (*) pathOptions
    let pathOptionsToSplit = List.map2 (*) pathOptions splitters
    splitPathOptions pathOptionsToSplit |> List.map2 (+) unchangedPathOptions

inputParsed[1..] 
    |> List.fold (fun pathOptions splitters -> getNextPathOptions pathOptions splitters) inputParsed[0] 
    |> List.sum
