open System; open type System.Convert

let rec safe direction report =
    if Set.count direction > 1 then false else
    match report with
    | [] -> true
    | (x, y) :: _ when abs (x - y) > 3 || abs (x - y) < 1 -> false
    | (x, y) :: tail -> safe (direction + set [Math.Sign(x - y)]) tail

let permute report = [ for i in 0 .. Seq.length report - 1 -> Seq.removeAt i report ]

let reports = IO.File.ReadAllLines "day02.txt" |> Seq.map (fun l -> l.Split() |> Seq.map int)

reports |> Seq.sumBy (Seq.pairwise >> Seq.toList >> safe Set.empty >> ToInt32) |> printfn "Part 1: %i"
reports |> Seq.sumBy (permute >> Seq.exists (Seq.pairwise >> Seq.toList >> safe Set.empty) >> ToInt32) |> printfn "Part 2: %i"
