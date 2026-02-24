open System
open System.Collections.Generic
open System.IO

let inputPath = "inputExample.txt"
//let inputPath = "input.txt"

let points = File.ReadAllLines inputPath 
                    |> Array.map (fun line -> 
                                line.Split [|','|] 
                                |> Array.map int64 
                                |> fun [|a;b|] -> (a,b))
                    |> Array.toList



let getHighestCombination (cx,cy) (rest:list<int64*int64>) =
    rest |> List.fold (fun acc (rx,ry) -> ((cx,cy) , (rx,ry),  ((abs (rx - cx ))+ 1L ) * ((abs(ry - cy )) + 1L))::acc) [] |> List.maxBy (fun (_,_,size) -> size)

let rec GetBestRectangles (points:list<int64*int64>) =
    match points with 
    | [] -> []
    | last::[] -> []
    | head::tail -> getHighestCombination head tail :: GetBestRectangles tail

GetBestRectangles points |> List.maxBy (fun (_,_,size) -> size)
