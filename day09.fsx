let input = System.IO.File.ReadAllText "day09.txt"

let expanded = 
    input.ToCharArray() 
    |> Array.mapi (fun i c ->
        let num = int $"{c}"
        if i % 2 <> 0 then Array.init num (fun _ -> None)
        else Array.init num (fun _ -> Some (i/2))
    ) |> Array.collect id

let defrag (arr : int option array) = 
    for i = 0 to arr.Length-1 do
        if arr[i] = None && arr |> Array.skip i |> Array.exists Option.isSome  then
            let last = arr |> Array.findIndexBack Option.isSome
            Array.set arr i arr[last]
            Array.set arr last None
    arr

let checksum (arr : int option array) =
    arr 
    |> Array.where Option.isSome 
    |> Array.map (Option.defaultValue 0) 
    |> Array.mapi (fun i id -> i*id)
    |> Array.fold (fun acc chk -> acc + (uint64 chk) ) 0UL

expanded |> defrag |> checksum |> printfn "%A"