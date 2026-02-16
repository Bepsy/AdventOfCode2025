open System
open System.Collections.Generic
open System.IO

//let inputPath = "inputExample.txt"
let inputPath = "input.txt"

type connection =
    { coordinates : list<int * int * int>
      distance : int64 } 

type network = list<int*int*int>

let coordinates = File.ReadAllLines inputPath 
                    |> Array.map (fun line -> 
                                line.Split [|','|] 
                                |> Array.map int 
                                |> fun [|a;b;c|] -> (a,b,c))
                    |> Array.toList

let calculateDistance c1 c2=
    let (sx,sy,sz) = c1
    let (tx, ty, tz) = c2
    let disx = Math.Pow (float(tx-sx), 2) 
    let disy = Math.Pow (float(ty-sy), 2)
    let disz = Math.Pow (float(tz-sz), 2)
    let distance = sqrt ( float (disx + disy + disz)) |> int64
    {
        coordinates = [c1;c2] 
        distance = distance
    }

let connections = coordinates |> List.mapi (fun i c1 ->
                                            coordinates[i+1..] |> List.map (fun c2 -> calculateDistance c1 c2)) 
                            |> List.concat
                            |> List.sortBy (fun c -> c.distance)
                            |> List.take 1000

let coordinateIsInNetwork network coordinate = network |> List.exists (fun c -> c = coordinate)

let FindNetwork networks coordinate = 
    networks |> List.tryFind (fun network -> coordinateIsInNetwork network coordinate)

let makeNetworks (connections:list<connection>) =
    let rec loop (result:list<network>) (restConnections:list<connection>) =
        match restConnections with
        | [] -> result
        | connection :: restConnections 
            ->  let networkC1Option = FindNetwork result connection.coordinates[0]
                let networkC2Option = FindNetwork result connection.coordinates[1]
                match networkC1Option, networkC2Option with 
                | None , None -> loop (connection.coordinates::result) restConnections
                | Some networkC1 , None  when not (coordinateIsInNetwork networkC1 connection.coordinates[1])
                    ->  let newNetwork = connection.coordinates[1] :: networkC1
                        let networks = result |> List.except [networkC1] 
                        loop (newNetwork::networks) restConnections
                | None , Some networkC2 when not (coordinateIsInNetwork networkC2 connection.coordinates[0])
                    ->  let newNetwork = connection.coordinates[0] :: networkC2
                        let networks = result |> List.except [networkC2] 
                        loop (newNetwork::networks) restConnections
                | Some networkC1 , Some networkC2 
                    ->  let newNetwork = networkC2 |> List.append networkC1 |> List.distinct
                        let networks = result |> List.except [networkC1; networkC2]
                        loop (newNetwork::networks) restConnections
                | _,_ -> loop result restConnections
    loop [] connections
    
let result = makeNetworks connections 
            |> List.map (fun network -> network.Length) 
            |> List.sortDescending 
            |> List.take 3 
            |> List.fold (fun acc elem -> acc*elem ) 1