let l, r = System.IO.File.ReadAllLines "inputs/day01.txt"
           |> Array.map (fun l -> l.Split("   ") |> Array.map int)
           |> (fun l -> l |> Array.map Array.head |> Array.sort, 
                        l |> Array.map Array.last |> Array.sort)
            
(l, r) ||> Array.map2 (-) |> Array.map abs |> Array.sum |> printfn "Part 1: %A"
l |> Array.map (fun i -> i * (r |> Array.filter (fun x -> x = i) |> Array.length)) |> Array.sum |> printfn "Part 2: %A"