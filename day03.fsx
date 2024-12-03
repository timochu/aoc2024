open System.Text.RegularExpressions
let input = System.IO.File.ReadAllText "day03.txt"
let mult = Regex(@"mul\(([0-9]|[1-9][0-9]|[1-9][0-9][0-9]),([0-9]|[1-9][0-9]|[1-9][0-9][0-9])\)")
let disabled = Regex(@"don't\(\).*?(do\(\)|$)", RegexOptions.Singleline)

input |> mult.Matches |> Seq.sumBy (fun m -> (int m.Groups[1].Value) * (int m.Groups[2].Value)) |> (printfn "Part 1: %i")
input |> (fun x -> disabled.Replace(x, "")) |> mult.Matches  |> Seq.sumBy (fun m -> (int m.Groups[1].Value) * (int m.Groups[2].Value)) |> printfn "Part 2: %A"