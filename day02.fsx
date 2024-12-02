open type System.Math
let rec safe direction report =
    if Set.count direction > 1 then 0
    else match report with
         | [] -> 1
         | (x,y) :: _ when abs(x-y) > 3 -> 0
         | (x,y) :: _ when abs(x-y) < 1 -> 0
         | (x,y) :: tail -> safe (direction |> Set.add (Sign(x-y))) tail

System.IO.File.ReadAllLines "day02.txt"
|> Array.sumBy (fun l -> l.Split " " |> Array.map int |> Array.toList |> List.pairwise |> safe Set.empty)
|> printfn "%A"
