let input = System.IO.File.ReadAllLines "day05.txt"
let rules = input |> Array.takeWhile (fun i -> i.Contains "|") |> Array.map (fun i -> int i[..1], int i[3..])

let hits pages =
    [ for (r1, r2) in rules do
        match Array.tryFindIndex ((=) r1) pages, Array.tryFindIndex ((=) r2) pages with
        | Some i1, Some i2 when i1 > i2 -> yield (r1, r2)
        | _ -> () ]

let rec swapper (rules : ((int*int) List)) (pages : int array) =
    match rules |> List.randomShuffle with // secret sauce 😙🤌
    | [] -> pages
    | rules -> (pages, rules) ||> List.fold (fun acc (r1, r2) -> 
                let i1, i2 = acc |> Array.findIndex ((=) r1), acc |> Array.findIndex ((=) r2)
                acc |> Array.updateAt i1 r2 |> Array.updateAt i2 r1)
            |> (fun p -> hits p, p) ||> swapper

let printable, unprintable = input |> Array.skipWhile (fun i -> i.Contains "," |> not) |> Array.map (fun i -> i.Split(',') |> Array.map int) |> Array.partition (hits >> List.isEmpty)
printable |> Array.sumBy (fun u -> u.[u.Length / 2]) |> printfn "Part 1: %i"
unprintable |> Array.map (fun u -> swapper (hits u) u) |> Array.sumBy (fun u -> u.[u.Length / 2]) |> printfn "Part 2: %i"