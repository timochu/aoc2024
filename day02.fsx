let rec safe direction report =
    if Set.count direction > 1 then false else
    match report with
    | [] -> true
    | (x, y) :: _ when abs (x - y) > 3 || abs (x - y) < 1 -> false
    | (x, y) :: tail -> safe (direction |> Set.add (System.Math.Sign(x - y))) tail
let permute (report : int list) = [ 0 .. report.Length - 1 ] |> List.map (fun i -> report |> List.removeAt i)

let reports = System.IO.File.ReadAllLines "day02.txt" |> Array.map (fun l -> l.Split() |> Array.map int |> Array.toList)
reports |> Array.filter (List.pairwise >> safe Set.empty) |> Array.length |> printfn "Part 1: %i"
reports |> Array.filter (permute >> List.exists (List.pairwise >> safe Set.empty)) |> Array.length |> printfn "Part 2: %i"
