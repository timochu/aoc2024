open System; open type System.Convert

let rec safe direction report =
    match report, direction with
    | _, d when d |> set |> Set.count > 1 -> false
    | [], _ -> true
    | (x, y) :: _, _ when abs (x - y) > 3 -> false
    | (x, y) :: _, _ when abs (x - y) < 1 -> false
    | (x, y) :: rest, d -> safe (Math.Sign(x - y) :: d) rest

let permute report = [ for i in 0 .. List.length report - 1 -> List.removeAt i report ]

let reports = [for report in IO.File.ReadAllLines "day02.txt" -> [for level in report.Split() -> int level]]

reports |> List.sumBy (List.pairwise >> safe [] >> ToInt32) |> printfn "Part 1: %i"
reports |> List.sumBy (permute >> List.exists (List.pairwise >> safe []) >> ToInt32) |> printfn "Part 2: %i"
