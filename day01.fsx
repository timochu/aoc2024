let left, right, lookup =
    System.IO.File.ReadAllLines "inputs/day01.txt"
    |> Array.map (fun line -> int line[..5], int line[8..])
    |> Array.unzip
    |> fun (left, right) -> Array.sort left, Array.sort right, right |> Array.countBy id |> Map

(left, right) ||> Array.map2 (-) |> Array.sumBy abs |> printfn "Part 1: %A"
left |> Array.sumBy (fun id -> id * (lookup.TryFind id |> Option.defaultValue 0)) |> printfn "Part 2: %A"
