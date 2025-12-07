open System
open System.IO

// let inputPath = "inputExample.txt"
let inputPath = "input.txt"

let input = File.ReadAllLines inputPath

let reverseString (s: string) =
    s.ToCharArray() |> Array.rev |> System.String

let maxJoltage maxLength (bank:string) =
    let rec loop (result:string) (bank:string) =
        let combinedLength = result.Length + bank.Length
        match result.Length with
        | _ when combinedLength = maxLength -> (reverseString result) + bank
        | 0 -> loop (string bank[0]) bank[1..]  
        | _ when int bank[0] > int result[0] -> loop result[1..] bank
        | _ when result.Length <> maxLength -> loop (string bank[0] + result) bank[1..] 
        | _ -> loop result bank[1..]
    loop "" bank

let maxJoltage_2 = maxJoltage 2
let maxJoltage_12 = maxJoltage 12

input |> Array.map maxJoltage_12 |> Array.map (fun s -> int64 s) |> Array.sum