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

let antinodes = 
    frequencies 
    |> List.collect (fun frequency ->
        antennas 
        |> List.where (fun a -> snd a = frequency)
        |> List.map fst
        |> combinations 2
        |> List.map (fun coords -> (fst coords[0], snd coords[0]), (fst coords[1], snd coords[1]))
        |> List.map (fun ((x1,y1),(x2,y2)) -> 
            let xdist = abs(x1 - x2)
            let ydist = abs(y1 - y2)
            match x1 < x2, y1 < y2 with
            | true, true   -> x1 - xdist, y1 - ydist
            | true, false  -> x1 - xdist, y1 + ydist
            | false, true  -> x1 + xdist, y1 - ydist
            | false, false -> x1 + xdist, y1 + ydist
        ))
    |> List.where (fun (x,y) -> x >= 0 && y >= 0 && x <= bound && y <= bound)
    |> List.distinct

let antinodes2 = 
    frequencies 
    |> List.collect (fun frequency ->
        antennas 
        |> List.where (fun a -> snd a = frequency)
        |> List.map fst
        |> combinations 2
        |> List.map (fun coords -> (fst coords[0], snd coords[0]), (fst coords[1], snd coords[1]))
        |> List.collect (fun ((x1,y1),(x2,y2)) -> 
            let xdists = [for i in 1..50 -> abs(x1 - x2) * i]
            let ydists = [for i in 1..50 -> abs(y1 - y2) * i]
            let dists = List.zip xdists ydists
            [   for (xd, yd) in dists ->
                match x1 < x2, y1 < y2 with
                | true, true   -> x1 - xd, y1 - yd
                | true, false  -> x1 - xd, y1 + yd
                | false, true  -> x1 + xd, y1 - yd
                | false, false -> x1 + xd, y1 + yd
            ]
            |> List.append [(x1,y1);(x2,y2)]
        ))
    |> List.where (fun (x,y) -> x >= 0 && y >= 0 && x <= bound && y <= bound)
    |> List.distinct


printfn "Part 1: %A" (antinodes |> List.length)
printfn "Part 2: %A" (antinodes2 |> List.length)

