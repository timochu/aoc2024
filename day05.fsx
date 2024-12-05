let input = System.IO.File.ReadAllLines "day05e.txt"
let rules = input |> Array.takeWhile (fun i -> i.Contains "|") |> Array.map (fun i -> int i[..1], int i[3..])
let updates = input |> Array.skipWhile (fun i -> i.Contains "," |> not) |> Array.map (fun i -> i.Split(',') |> Array.map int)

let hits (pages : int array) : (int * int) seq = 
    seq { 
        for (r1, r2) in rules do
        if pages |> Array.contains r1 && pages |> Array.contains r2 &&
            (pages |> Array.findIndex ((=) r1)) > (pages |> Array.findIndex ((=) r2)) then
                yield r1, r2
    }

updates |> Array.where (hits >> Seq.isEmpty) |> Array.sumBy (fun u -> u.[u.Length/2]) |> printfn "Part 1: %i"
