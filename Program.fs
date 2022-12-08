open System.IO

let lines = File.ReadAllLines "/tmp/aoc/input" |> Array.toList

lines |> List.map (printfn "%A")
let treerows = lines |> List.map (fun trees -> trees.ToCharArray() |> Array.map (fun t -> int t - int '0') |> Array.toList)

let visibility = treerows |> List.map (List.map (fun n -> (n,false)))


let lowerVisible (row: (int*bool) list) =
    let rec lower (h:int) (row:(int*bool) list) =
        match row with
        | [] -> []
        | (n,_)::rest when h < n -> (n,true)::(lower n rest)
        | (n,v)::rest -> (n,v) :: lower h rest
    let row = lower -1 row |> List.rev
    let row = lower -1 row |> List.rev
    row
    
let visibleRows = visibility |> List.map lowerVisible
let columns = visibleRows |> List.transpose
let visibleCols = columns |> List.map lowerVisible

let visible = visibleCols |> List.transpose 
    
// visible |> List.map (printfn ("%A"))

visible |> List.concat |> List.filter (fun (a,b) -> b) |> List.length |> printfn "%A"

let distanceRows = treerows |> List.map (List.map (fun n -> (n,1)))

let rec treeViews2 (row: (int*int) list) (followsLarge : bool) (beforeLarge : bool) =
    if row = [] then []
    else
        let tallest = row |> List.maxBy fst
        let index = row |> List.findIndex (fun n -> n = tallest)
        let before = row |> List.take index
        let tallest = row |> List.skip index |> List.head
        let after = row |> List.skip (index + 1)
        let beforeViews = before.Length + (if followsLarge then 1 else 0)
        let afterViews =
                    match after |> List.tryFindIndex (fun n -> (fst n) = (fst tallest)) with
                    | None -> after.Length + (if beforeLarge then 1 else 0)
                    | Some(n) -> n + 1
        let views = beforeViews * afterViews 
        let (h,v) = tallest
        let tallest2 = (h,v*views)
        let before = treeViews2 before followsLarge true 
        let after = treeViews2 after true beforeLarge
        printfn $"tallest: {tallest} {tallest2} before={beforeViews} after={afterViews} #[${followsLarge} ]#${beforeLarge} follows={after}"
        List.concat [before;[tallest2];after]
let treeViews (row: (int*int) list) = treeViews2 row false false 
        
let viewRows2 = distanceRows
               |> List.map treeViews
viewRows2 |> List.map (printfn "%A")
printfn "XXX"

let viewRows = viewRows2
               |> List.transpose
               |> List.map treeViews
               |> List.transpose
     

let scenic = viewRows |> List.concat |> List.map snd |> List.max 

viewRows |> List.map (printfn "%A")

printfn $"{scenic}" 