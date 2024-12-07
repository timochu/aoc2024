let input = 
    [for l in System.IO.File.ReadAllLines "day07.txt" -> 
        [for n in l.Split([|' '; ':'|], System.StringSplitOptions.RemoveEmptyEntries) -> 
            uint64 n]] 
    |> List.map (fun n -> n[0], n[1..])

let (|||) (a: uint64) (b: uint64) : uint64 = $"{a}{b}" |> uint64

let rec combinations n items =
    if n = 0 then [[]]
    else
        [ for x in items do
            for xs in combinations (n - 1) items do
                yield x :: xs ]

let possible (operators: list<(uint64 -> uint64 -> uint64)>) (input : uint64 * uint64 list) =
    let expected, numbers = fst input, snd input |> List.indexed
    combinations (numbers.Length-1) operators 
    |> List.map (fun ops -> numbers |> List.reduce (fun (i1, a) (i2, b) -> i2, ops[i1] a b) |> snd) 
    |> List.contains expected

input |> List.where (possible [(+) ; (*)])        |> List.sumBy fst |> printfn "Part 1: %i"
input |> List.where (possible [(+) ; (*); (|||)]) |> List.sumBy fst |> printfn "Part 2: %i"
