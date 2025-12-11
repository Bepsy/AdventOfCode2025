open System
open System.IO

//let inputPath = "inputExample.txt"
let inputPath = "input.txt"

let input = File.ReadAllLines inputPath 

let grid =  input |> Array.map (fun line -> line.ToCharArray() |> Array.map (fun c -> if c = '@' then 1 else 0))

let paddingRow = [|Array.init input[0].Length (fun c -> 0)|]
let gridWithPadding =  Array.append grid paddingRow 
                        |> Array.append paddingRow 
                        |> Array.map (fun row -> Array.append row [|0|] |> Array.append [|0|] )

let checkRol row column (grid:int array array) =
    match grid[row][column] with
    | 0 -> 0
    | _ ->  
        let sum = grid[(row - 1)..(row + 1)] |> Array.map (fun line ->  Array.sum line[(column - 1)..(column + 1)]) |> Array.sum
        if sum-1 < 4 then 0 else 1

let checkGrid (grid:int array array) =
    let isValidPos pos array = pos > 0 && pos < Array.length array - 1   
    let isValidPos2D rowi coli = isValidPos rowi grid && isValidPos coli grid[0]  
    grid |> Array.mapi (fun rowi row -> row |> Array.mapi (fun coli rol -> if rol = 1 && isValidPos2D rowi coli then checkRol rowi coli grid else 0))

let getRolls grid = grid |> Array.map (fun line -> Array.filter (fun rol -> rol = 1) line |> Array.length) |> Array.sum

let startRollsCount = getRolls gridWithPadding
let remainingRollsCount = checkGrid gridWithPadding |> Array.map (fun line -> Array.filter (fun rol -> rol = 1) line |> Array.length) |> Array.sum
let result1 = startRollsCount - remainingRollsCount

let checkGridRecursive (startGrid:int array array) =
    let startRollsCount = getRolls startGrid
    let rec loop rollsCount currentGrid =
        let checkedGrid = checkGrid currentGrid 
        let newRollsCount = getRolls checkedGrid
        if newRollsCount = rollsCount then newRollsCount else loop newRollsCount checkedGrid
    loop startRollsCount startGrid

let result2 = startRollsCount - checkGridRecursive gridWithPadding
