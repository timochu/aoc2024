open System; open type System.Convert

let rec safe direction report =
    match direction, report  with
    | dir, _ when dir |> Set.count > 1  -> false
    | _, []                             -> true
    | _, (x, y) :: _ when abs (x-y) > 3 -> false
    | _, (x, y) :: _ when abs (x-y) < 1 -> false
    | dir, (x, y) :: rest               -> safe (dir.Add (Math.Sign(x - y))) rest

let permute report = [ for i in 0 .. List.length report - 1 -> List.removeAt i report ]

let reports = [for report in IO.File.ReadAllLines "day02.txt" -> [for level in report.Split() -> int level]]

reports |> List.sumBy (List.pairwise >> safe Set.empty >> ToInt32) |> printfn "Part 1: %i"
reports |> List.sumBy (permute >> List.exists (List.pairwise >> safe Set.empty) >> ToInt32) |> printfn "Part 2: %i"
