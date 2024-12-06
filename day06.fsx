let input = [for l in System.IO.File.ReadAllLines "day06.txt" -> [for c in l -> c]] 
let bound = input.Length - 1 
let map = input |> List.mapi (fun y line -> line |> List.mapi (fun x c -> (x,y), c)) |> List.collect id
let obstacles = map |> List.where (snd >> ((=) '#')) |> List.map fst
let guard = map |> List.where (snd >> ((=) '^')) |> List.head

let step guard =
    match guard with
    | (x,y), '^' -> (x, y-1), '^'
    | (x,y), '>' -> (x+1, y), '>'
    | (x,y), 'v' -> (x, y+1), 'v'
    | (x,y), _   -> (x-1, y), '<'

let turn guard =
    match guard with
    | pos, '^' -> pos, '>'
    | pos, '>' -> pos, 'v'
    | pos, 'v' -> pos, '<'
    | pos, _   -> pos, '^'

let repetitive take (acc : ((int*int) list)) =
    if acc |> List.length < take then -1 else
    let last = acc |> List.take take
    let windows = acc |> List.windowed take
    let repetitions =
        windows
        |> List.where (fun window ->
            let same = List.forall2 (=) window last
            same)
        |> List.length
    repetitions

let rec plot (acc : ((int*int) list)) obstacles (guard : (int * int) * char) =
    if acc |> (repetitive 5) > 1 then [] else
    match guard with
    | (x,_), _ when x < 0      -> acc
    | (x,_), _ when x >= bound -> acc
    | (_,y), _ when y < 0      -> acc
    | (_,y), _ when y >= bound -> acc
    | _ ->
        let pos, dir = step guard
        if obstacles |> List.contains pos then plot acc obstacles (turn guard)
        else plot (pos::acc) obstacles (pos, dir)

let route = guard |> plot [] obstacles |> List.distinct
route |> List.length |> printfn "Part 1: %i"
route |> Array.ofList |> Array.Parallel.map (fun e -> guard |> plot [] (e::obstacles)) |> Array.where List.isEmpty |> Array.length |> printfn "Part 2: %i"