open System
open System.Collections.Generic
open System.IO

//let inputPath = "inputExample.txt"
let inputPath = "input.txt"

type connection =
    { coordinates : Set<int64 * int64 * int64>
      distance : int64 } 

let coordinates = File.ReadAllLines inputPath 
                    |> Array.map (fun line -> 
                                line.Split [|','|] 
                                |> Array.map int64 
                                |> fun [|a;b;c|] -> (a,b,c))
                    |> Array.toList

let calculateDistance (x1, y1, z1) (x2, y2, z2) =
    let dx = float (x1 - x2)
    let dy = float (y1 - y2)
    let dz = float (z1 - z2)
    
    let dist = sqrt (dx*dx + dy*dy + dz*dz) |> int64
    {
        coordinates = set [(x1, y1, z1); (x2, y2, z2)]
        distance = dist
    }

let connections = coordinates |> List.mapi (fun i c1 ->
                                            coordinates[i+1..] |> List.map (fun c2 -> calculateDistance c1 c2)) 
                            |> List.concat
                            |> List.sortBy (fun c -> c.distance)
                            |> List.map (fun c -> c.coordinates)

let makeNetworks (connections:list<Set<int64*int64*int64>>) =
    let rec loop result (circuits:list<Set<int64*int64*int64>>) (restConnections:list<Set<int64*int64*int64>>) =
        match restConnections with
        | [] -> result
        | current::tail ->  
                let rest, found = circuits |> List.partition (fun s -> s |> Set.intersect current |> Set.isEmpty)
                let newResult = 
                    match rest.Length, found.Length with
                    | 0, 2 -> current  
                    | 0, 1 when not (Set.isSubset current found[0]) -> current  
                    | _, _ -> result
                let mergeFound = current :: found |> seq |> Set.unionMany
                let newcircuits = mergeFound :: rest
                loop newResult newcircuits tail
    loop Set.empty List.Empty connections 

let result = makeNetworks connections |> Set.toList |> List.fold (fun acc (x,y,z) -> x * acc ) 1L