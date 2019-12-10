open System.IO

module Day2Solution =
    // this solution expects all input to be on a single line
    let readFileLine (filePath:string) = Seq.toList(File.ReadLines(filePath)).[0]

    let parseStringToIntArray (str:string) = str.Split ',' |> Array.map (fun x -> x |> int)

[<EntryPoint>]
// TESTING: prints contents of parsed array
let main argv =        
    for i in Day2Solution.parseStringToIntArray (Day2Solution.readFileLine argv.[0]) do printfn "%s" (i.ToString())    
    0 // return an integer exit code