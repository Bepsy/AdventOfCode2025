open System
open System.IO

let inputPath = "input.txt"
// let inputPath = "Input.txt"

let input = File.ReadAllText inputPath

let inputParsed = input.Split [|',';'\r'; '\n'|] 
                |> Array.filter (fun s -> s <> "") 
                |> Array.map (fun s -> (s.Split [|'-'|]))
                |> Array.map (fun arr -> int64 arr[0], int64 arr[1])  


let getSplitsizes length =
    let rec getDivisions result current =
        match length with
        | _ when current > length -> result   
        | _ when length%current = 0 -> getDivisions (current::result) (current+1)  
        | _ -> getDivisions result (current+1) 
    getDivisions [] 2 |> List.map (fun div -> length/div)

let checkSequenceForSplitSize splitsize (sequence:string)  =
    let rec loop result seq =
        match (seq:string) with
        | _ when seq.Length = 0 -> result
        | _ -> loop (seq[0 .. splitsize - 1]::result) seq.[splitsize  .. ] 
    let splittedSequence = loop [] sequence
    splittedSequence |> List.forall (fun s -> s = splittedSequence[0])

let checkSequence (sequence:string) = getSplitsizes sequence.Length 
                                    |> List.map (fun elem -> checkSequenceForSplitSize elem sequence)  
                                    |> List.contains true
                                    |> fun isvalid -> if isvalid then int64 sequence else 0 

let loopRange (current, endRange) =
    let rec loop prevResult (current) (endRange) =
        let result = (checkSequence (string current)) + prevResult
        match current with
        | _ when current >= endRange -> result
        | _  -> loop result (current + 1L) endRange
    loop 0L current endRange

#time
let result = inputParsed |> Array.map loopRange |> Array.sum
#time
