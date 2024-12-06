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

let repetitive (acc : (((int * int) * char) list)) =
    if acc.IsEmpty then false else
    acc.Tail |> List.exists ((=) acc.Head) 

let rec plot (acc : (((int * int) * char) list)) obstacles (guard : (int * int) * char) =
    if acc |> repetitive then [] else
    match guard |> step with
    | (x,_), _ when x < 0 || x > bound -> acc
    | (_,y), _ when y < 0 || y > bound -> acc
    | position, direction ->
        if obstacles |> List.contains position then
            plot acc obstacles (turn guard)
        else 
            plot ((position, direction)::acc) obstacles (position, direction)

let route = guard |> plot [] obstacles |> List.map fst |> List.distinct
route |> List.length |> printfn "Part 1: %i"
route |> Array.ofList |> Array.Parallel.map (fun (e) -> guard |> plot [] (e::obstacles)) |> Array.where List.isEmpty |> Array.length |> printfn "Part 2: %i"