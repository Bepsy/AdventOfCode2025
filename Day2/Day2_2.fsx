open System
open System.IO

let inputPath = "input.txt"
// let inputPath = "Input.txt"

let input = File.ReadAllText inputPath

let inputParsed = input.Split [|',';'\r'; '\n'|] 
                |> Array.filter (fun s -> s <> "") 
                |> Array.map (fun s -> (s.Split [|'-'|]))
                |> Array.map (fun arr -> int64 arr[0], int64 arr[1])  

let checkSequenceForSplitSize splitsize (sequence:string)  =
    let rec loop result seq =
        match (seq:string) with
        | _ when seq.Length = 0 -> result
        | _ -> loop (seq[0 .. splitsize - 1]::result) seq.[splitsize  .. ] 
    let splittedSequence = loop [] sequence
    splittedSequence |> List.forall (fun s -> s = splittedSequence[0])

let checkSequence (sequence:string) =
    let rec loop splitsize (sequence:string) =
        match splitsize with
        | _ when (sequence.Length)%splitsize <> 0 -> loop (splitsize+1) sequence
        | _ when splitsize = sequence.Length -> 0L
        | _ when checkSequenceForSplitSize splitsize sequence -> int64 sequence
        | _ -> loop (splitsize+1) sequence
    loop 1 sequence

let loopRange (current, endRange) =
    let rec loop prevResult (current) (endRange) =
        let result = (checkSequence (string current)) + prevResult
        match current with
        | _ when current >= endRange -> result
        | _  -> loop result (current + 1L) endRange
    loop 0L current endRange

let result = inputParsed |> Array.map loopRange |> Array.sum