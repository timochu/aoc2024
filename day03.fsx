open System.Text.RegularExpressions
let memory = System.IO.File.ReadAllText "day03.txt"
let rx = Regex(@"mul\(([0-9]|[1-9][0-9]|[1-9][0-9][0-9]),([0-9]|[1-9][0-9]|[1-9][0-9][0-9])\)", RegexOptions.Compiled)
memory |> rx.Matches |> Seq.sumBy (fun m -> (int m.Groups[1].Value) * (int m.Groups[2].Value)) |> (printfn "Part 1: %i")
