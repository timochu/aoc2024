open System.IO; open System.Text.RegularExpressions

let mult = Regex(@"mul\((\d{1,3}),(\d{1,3})\)").Matches >> Seq.sumBy (fun m -> int m.Groups[1].Value * int m.Groups[2].Value)
let clean s = Regex(@"don't\(\).*?(do\(\)|$)", RegexOptions.Singleline).Replace(s, "")

File.ReadAllText "day03.txt" |> mult |> printfn "Part 1: %i"
File.ReadAllText "day03.txt" |> clean |> mult |> printfn "Part 2: %A"