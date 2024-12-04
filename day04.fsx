open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines "day04.txt" |> Array.map (fun s -> s.ToCharArray())

let xmas (c : char array) = Regex("(?=(XMAS|SAMX))").Matches(System.String c).Count
let mas (c : char array) = Regex("MAS|SAM").Match(System.String c) <> Match.Empty

let diagonals (arr: 'a[][]) : 'a[][] =
    let N = arr.Length
    [|
        for k in 0 .. 2 * (N - 1) do
            let yMin = max 0 (k - (N - 1))
            let yMax = min (N - 1) k
            [|
                for y in yMin .. yMax do
                    let x = k - y
                    arr.[y].[x]
            |]
    |]

let window2D (size: int) (arr: 'a[][]) : 'a[][][] =
    let rows = arr.Length
    let cols = arr.[0].Length
    [|
        for i in 0 .. rows - size do
            for j in 0 .. cols - size do
                [|
                    for x in 0 .. size - 1 do
                        [|
                            for y in 0 .. size - 1 do
                                arr.[i + x].[j + y]
                        |]
                |]
    |]

[
    input |> Array.sumBy xmas
    input |> Array.transpose |> Array.sumBy xmas
    input |> diagonals |> Array.sumBy xmas
    input |> Array.map Array.rev |> diagonals |> Array.sumBy xmas
] 
|> List.sum 
|> printfn "Part 1: %i"

window2D 3 input 
|> Array.where (fun f -> 
    [|f |> diagonals ; f |> Array.rev |> diagonals |] 
    |> Array.collect id 
    |> Array.filter (fun x -> Array.length x = 3) 
    |> Array.forall mas)
|> Array.length
|> printfn "Part 2: %i"