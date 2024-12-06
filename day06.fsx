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

let rec plot (acc : ((int*int) list)) (guard : (int * int) * char) =
    match guard with
    | (x,_), _ when x < 0     -> acc
    | (x,_), _ when x >= bound -> acc
    | (_,y), _ when y < 0     -> acc
    | (_,y), _ when y >= bound -> acc
    | _ ->
        let pos, dir = step guard
        if obstacles |> List.contains pos then plot acc (turn guard)
        else plot (pos::acc) (pos, dir)


guard |> plot [] |> List.distinct |> List.length |> printfn "Part 1: %i"