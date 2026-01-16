open System
open System.Collections.Generic
open System.IO

let inputPath = "input.txt"
// let inputPath = "inputExample.txt"
let input = File.ReadAllLines inputPath

let inputParsed = input 
                |> Array.map (fun line -> 
                    line |> 
                    Seq.mapi(fun column c -> 
                        if c = '^' then Some column else None )
                    |> Seq.choose id
                    |> Seq.toArray)
                |> Array.filter (fun line -> line.Length > 0)

let getNextBeams currentBeams splitters =
    let clashingBeams = currentBeams |> Array.filter (fun c -> splitters |> Array.contains (c))
    let notClashingBeams = currentBeams |> Array.except clashingBeams
    let newBeams = clashingBeams |> Array.map (fun beam -> [|beam-1;beam+1|]) |> Array.concat
    notClashingBeams |> Array.append newBeams |> Array.distinct

let getNextSplits currentSplits Beams splitters = Beams 
                                                    |> Array.filter (fun c -> splitters |> Array.contains (c)) 
                                                    |> Array.length 
                                                    |> (+) currentSplits 

inputParsed |> Array.fold (fun (accBeams,accSplits) splitters -> 
                                            getNextBeams accBeams splitters, getNextSplits accSplits accBeams splitters)
                                        ([|70|], 0)
