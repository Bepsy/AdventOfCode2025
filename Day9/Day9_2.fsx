open System
open System.Collections.Generic
open System.IO

// let inputPath = "inputExample.txt"
let inputPath = "input.txt"

type Point = 
    {
        x : int64 
        y : int64
    }

type VerticalLine = 
    { x : int64; y1 : int64; y2 : int64 }

type HorizontalLine = 
    { y : int64; x1 : int64; x2 : int64 }

type Line = 
    | Vertical of VerticalLine
    | Horizontal of HorizontalLine

let redTiles = File.ReadAllLines inputPath 
                    |> Array.map (fun line -> 
                                line.Split [|','|] 
                                |> Array.map int64 
                                |> fun [|a;b|] -> {x=a; y=b})
                    |> Array.toList

let (horizontalLines, verticalLines) = 
    redTiles 
    |> List.windowed 2 
    |> List.fold (fun (hAcc, vAcc) pair ->
        let p1, p2 = pair.[0], pair.[1]
        
        if p1.x = p2.x then
            let vLine = { x = p1.x; y1 = min p1.y p2.y; y2 = max p1.y p2.y }
            (hAcc, vLine :: vAcc)
        else
            let hLine = { y = p1.y; x1 = min p1.x p2.x; x2 = max p1.x p2.x }
            (hLine :: hAcc, vAcc)
    ) ([], [])

let VerticalLineIsValid (vl:VerticalLine) (hlines:list<HorizontalLine>) =
    let PossibleEncapsulatingLines = hlines |> List.filter (fun hl -> hl.x1 <= vl.x && hl.x2 >= vl.x)
    PossibleEncapsulatingLines |> List.exists (fun hl -> hl.y >= vl.y2) 
    && PossibleEncapsulatingLines |> List.exists (fun hl -> hl.y <= vl.y1)
    && not (PossibleEncapsulatingLines |> List.exists (fun hl -> hl.y > vl.y1 && hl.y < vl.y2))

let HorizontalLineIsValid (hl:HorizontalLine) (vlines:list<VerticalLine>) =
    let PossibleEncapsulatingLines = vlines |> List.filter (fun vl -> vl.y1 <= hl.y && vl.y2 >= hl.y)
    PossibleEncapsulatingLines |> List.exists (fun vl -> vl.x >= hl.x2) 
    && PossibleEncapsulatingLines |> List.exists (fun vl -> vl.x <= hl.x1)
    && not (PossibleEncapsulatingLines |> List.exists (fun vl -> vl.x > hl.x1 && vl.x < hl.x2))

let RectangleIsValid (greenTileCircVlines: list<VerticalLine>, greentileCircHlines: list<HorizontalLine>) (rectangleVLines:list<VerticalLine>, rectangleHLines:list<HorizontalLine>) =
    rectangleVLines |> List.forall (fun vl -> VerticalLineIsValid vl greentileCircHlines) && rectangleHLines |> List.forall (fun hl -> HorizontalLineIsValid hl greenTileCircVlines)                                                           

let diagonalToSides (redTile1:Point, redTile2:Point) =
    let verticalLines = 
        [{x=redTile1.x ;y1 = min redTile1.y redTile2.y; y2 = max redTile1.y redTile2.y }
        ; {x=redTile2.x ;y1 = min redTile1.y redTile2.y; y2 = max redTile1.y redTile2.y }]
    let horizontalLines = 
        [{y=redTile1.y ;x1 = min redTile1.x redTile2.x; x2 = max redTile1.x redTile2.x }
        ; {y=redTile2.y ;x1 = min redTile1.x redTile2.x; x2 = max redTile1.x redTile2.x }]
    verticalLines, horizontalLines

let calculateSize (redtile1:Point, redtile2:Point) =
     (abs (redtile1.x - redtile2.x ) + 1L)  * (abs(redtile1.y - redtile2.y ) + 1L)

let getRectanglesSorted (redTiles:list<Point>) =
    let flatten listOfLists = listOfLists |> List.collect id
    let rec helper (redTiles:list<Point>) =
        match redTiles with 
        | [] -> []
        | last::[] -> []
        | head::tail ->  let combinationsHead = tail |> List.fold (fun acc current -> (head, current, calculateSize (head,current))::acc) []
                         combinationsHead :: helper tail    
    helper redTiles |> flatten   
                    |> List.sortBy (fun (_,_,size) -> size) 
                    |> List.map (fun (p1,p2,_) -> p1,p2) 
                    |> List.rev 

let result = getRectanglesSorted redTiles 
                |> List.find (fun rectangle -> diagonalToSides rectangle |> RectangleIsValid (verticalLines, horizontalLines))
                |> calculateSize
