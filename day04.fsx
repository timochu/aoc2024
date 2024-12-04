open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines "day04.txt" |> Array.map (fun s -> s.ToCharArray())

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

let xmas (c : char array) = Regex("(?=(XMAS|SAMX))").Matches(System.String c).Count

[
    input |> Array.sumBy xmas
    input |> Array.transpose |> Array.sumBy xmas
    input |> diagonals |> Array.sumBy xmas
    input |> Array.map Array.rev |> diagonals |> Array.sumBy xmas
] |> List.sum |> printfn "Part 1: %i"