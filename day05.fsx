let input = System.IO.File.ReadAllLines "day05.txt"
let rules = input |> Array.takeWhile (fun i -> i.Contains "|") |> Array.map (fun i -> int i[..1], int i[3..])
let updates = input |> Array.skipWhile (fun i -> i.Contains "," |> not) |> Array.map (fun i -> i.Split(',') |> Array.map int)

let hits (pages: int array) =
    pages, [ for (r1, r2) in rules do
               match Array.contains r1 pages, Array.contains r2 pages with
               | (true, true) when (Array.findIndex ((=) r1) pages) > (Array.findIndex ((=) r2) pages) ->
                   yield (r1, r2)
               | _ -> () ]

let rec sorter (pages : int array) (rules : ((int*int) List)) =
    match rules |> List.randomShuffle with // secret sauce 😙🤌
    | [] -> pages
    | rules ->
        for (r1,r2) in rules do
            let i1 = pages |> Array.findIndex ((=) r1)
            let i2 = pages |> Array.findIndex ((=) r2)
            Array.set pages i1 r2 
            Array.set pages i2 r1
        sorter pages (hits pages |> snd)
        
updates |> Array.where (hits >> snd >> List.isEmpty) |> Array.sumBy (fun u -> u.[u.Length / 2]) |> printfn "Part 1: %i"
updates 
|> Array.map hits 
|> Array.where (snd >> Seq.isEmpty >> not)
|> Array.map (fun (p, r) -> sorter p r)
|> Array.sumBy (fun u -> u.[u.Length / 2]) |> printfn "Part 2: %i"