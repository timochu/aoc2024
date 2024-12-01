let left, right = 
    System.IO.File.ReadAllLines "day01.txt"
    |> Array.map (fun l -> int l[..5], int l[8..])
    |> Array.unzip

(Array.sort left, Array.sort right)
||> Array.map2 (-)
|> Array.sumBy abs
|> printfn "Part 1: %i"

left 
|> Array.sumBy (fun id -> id * (right |> Array.filter ((=) id)).Length)
|> printfn "Part 2: %i"
