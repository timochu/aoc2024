let input = System.IO.File.ReadAllLines "day05.txt"
let rules = input |> Array.takeWhile (fun i -> i.Contains "|") |> Array.map (fun i -> int i[..1], int i[3..])
let updates = input |> Array.skipWhile (fun i -> i.Contains "," |> not) |> Array.map (fun i -> i.Split(',') |> Array.map int)

let isCorrect (pages : int array) = 
    rules |> Array.forall (fun (r1, r2) -> 
        if pages |> Array.contains r1 && pages |> Array.contains r2 then
            (pages |> Array.findIndex ((=) r1)) < (pages |> Array.findIndex ((=) r2))
        else true
    )

updates |> Array.where isCorrect |> Array.sumBy (fun u -> u.[u.Length/2]) |> printfn "Part 1: %i"