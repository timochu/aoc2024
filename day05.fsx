let input = System.IO.File.ReadAllLines "day05.txt"
let rules = [for l in input[..1175] -> int l[..1], int l[3..]]

let hits pages =
    [ for (r1, r2) in rules do
        match List.tryFindIndex ((=) r1) pages, List.tryFindIndex ((=) r2) pages with
        | Some i1, Some i2 when i1 > i2 -> yield (r1, r2)
        | _ -> () ]

let rec swap pages =
    (pages, rules) 
    ||> List.fold (fun acc (r1, r2) -> 
        match (acc |> List.tryFindIndex ((=) r1), acc |> List.tryFindIndex ((=) r2)) with 
        | Some i1, Some i2 when i1 > i2 -> acc |> List.updateAt i1 r2 |> List.updateAt i2 r1
        | _ -> acc)
    |> fun reordered -> if List.forall2 (=) pages reordered then reordered else swap reordered

let middle (arr : 'a list) = arr.[arr.Length / 2]
let printable, unprintable = [for l in input[1177..] -> [for i in l.Split(',') -> int i]] |> List.partition (hits >> List.isEmpty)
printable |> List.sumBy middle |> printfn "Part 1: %i"
unprintable |> List.sumBy(swap >> middle) |> printfn "Part 2: %i"