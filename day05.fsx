let input = System.IO.File.ReadAllLines "day05.txt"
let rules = input |> Array.takeWhile (fun i -> i.Contains "|") |> Array.map (fun i -> int i[..1], int i[3..])
let updates = input |> Array.skipWhile (fun i -> i.Contains "," |> not) |> Array.map (fun i -> i.Split(',') |> Array.map int)

let hits pages =
    [ for (r1, r2) in rules do
        match Array.tryFindIndex ((=) r1) pages, Array.tryFindIndex ((=) r2) pages with
        | Some i1, Some i2 when i1 > i2 -> yield (r1, r2)
        | _ -> () ]

let sorter pages  =
    let rec swapper rules pages =
        match rules |> List.randomShuffle with // secret sauce 😙🤌
        | [] -> pages
        | rules ->
            for (r1,r2) in rules do
                let i1 = pages |> Array.findIndex ((=) r1)
                let i2 = pages |> Array.findIndex ((=) r2)
                Array.set pages i2 r1
                Array.set pages i1 r2
            swapper (hits pages) pages
    swapper (hits pages) pages

updates |> Array.where (hits >> List.isEmpty) |> Array.sumBy (fun u -> u.[u.Length / 2]) |> printfn "Part 1: %i"
updates |> Array.where (hits >> Seq.isEmpty >> not) |> Array.map sorter |> Array.sumBy (fun u -> u.[u.Length / 2]) |> printfn "Part 2: %i"