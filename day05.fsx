let input = System.IO.File.ReadAllLines "day05.txt"
let rules = [for l in input[..1175] -> int l[..1], int l[3..]]

let hits pages =
    [ for (r1, r2) in rules do
        match List.tryFindIndex ((=) r1) pages, List.tryFindIndex ((=) r2) pages with
        | Some i1, Some i2 when i1 > i2 -> yield (r1, r2)
        | _ -> () ]

let rec swapper (rules : ((int*int) list)) (pages : int list) =
    match rules |> List.randomShuffle with // secret sauce 😙🤌
    | [] -> pages
    | rules -> (pages, rules) 
               ||> List.fold (fun acc (r1, r2) -> (acc |> List.findIndex ((=) r1), acc |> List.findIndex ((=) r2)) |> fun (i1, i2) -> acc |> List.updateAt i1 r2 |> List.updateAt i2 r1)
               |> (fun p -> hits p, p) ||> swapper

let printable, unprintable = [for l in input[1177..] -> [for i in l.Split(',') -> int i]] |> List.partition (hits >> List.isEmpty)
printable |> List.sumBy (fun u -> u.[u.Length / 2]) |> printfn "Part 1: %i"
unprintable |> List.map (fun u -> swapper (hits u) u) |> List.sumBy (fun u -> u.[u.Length / 2]) |> printfn "Part 2: %i"