open System

let rec safe direction report =
    if Set.count direction > 1 then false else
    match report with
    | [] -> true
    | (x, y) :: _ when abs (x - y) > 3 || abs (x - y) < 1 -> false
    | (x, y) :: tail -> safe (direction + set [Math.Sign(x - y)]) tail
    
let permute report = [ 0 .. Seq.length report - 1 ] |> List.map (fun i -> report |> List.removeAt i)

let reports = IO.File.ReadAllLines "day02.txt" |> Array.map (fun l -> l.Split() |> Array.map int |> Array.toList)

reports |> Seq.sumBy (List.pairwise >> safe Set.empty >> Convert.ToInt32) |> printfn "Part 1: %i"
reports |> Seq.sumBy (permute >> List.exists (List.pairwise >> safe Set.empty) >> Convert.ToInt32) |> printfn "Part 2: %i"
