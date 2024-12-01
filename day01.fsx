let (l, r) =
    System.IO.File.ReadAllLines "inputs/day01.txt"
    |> Array.map (fun l -> int l[..5], int l[8..])
    |> Array.unzip
    |> (fun (left, right) -> Array.sort left, Array.sort right)

(l, r)
||> Array.map2 (-)
|> Array.sumBy abs
|> printfn "Part 1: %A"

l
|> Array.sumBy (fun i -> i * (r |> Array.filter ((=) i) |> Array.length))
|> printfn "Part 2: %A"
