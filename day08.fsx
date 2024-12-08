let input = [for l in System.IO.File.ReadAllLines "day08.txt" -> [for c in l -> c]] 
let bound = input.Length - 1 
let map = input |> List.mapi (fun y line -> line |> List.mapi (fun x c -> (x,y), c)) |> List.collect id
let frequencies = input |> List.collect id |> List.where ((<>) '.') |> List.where ((<>) '#') |> List.distinct
let antennas = map |> List.where (snd >> (fun c -> frequencies |> List.contains c))

let rec combinations n items =
    if n = 0 then [[]]
    else
        [ for x in items do
            let remainingItems = List.filter ((<>) x) items
            for xs in combinations (n - 1) remainingItems do
                yield x :: xs ]

let antinodes distance self = 
    frequencies 
    |> List.collect (fun frequency ->
        antennas 
        |> List.where (fun a -> snd a = frequency)
        |> List.map fst
        |> combinations 2
        |> List.map (fun coords -> (fst coords[0], snd coords[0]), (fst coords[1], snd coords[1]))
        |> List.collect (fun ((x1,y1),(x2,y2)) -> 
            let result =
                [ for (xd, yd) in [for i in 1..distance -> abs(x1 - x2) * i, abs(y1 - y2) * i] ->
                  match x1 < x2, y1 < y2 with
                  | true, true   -> x1 - xd, y1 - yd
                  | true, false  -> x1 - xd, y1 + yd
                  | false, true  -> x1 + xd, y1 - yd
                  | false, false -> x1 + xd, y1 + yd ]
            if self then result |> List.append [(x1,y1);(x2,y2)]
            else result
        ))
    |> List.where (fun (x,y) -> x >= 0 && y >= 0 && x <= bound && y <= bound)
    |> List.distinct
    |> List.length

antinodes 1 false |> printfn "Part 1: %i"
antinodes 50 true |> printfn "Part 2: %i"

