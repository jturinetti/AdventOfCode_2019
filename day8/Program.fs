open System
open System.IO

module Day8Solution = 

    let readFileLine (filePath:string) = Seq.toList(File.ReadLines(filePath)).[0]

    let rec splitLayers (input:string, layerSize:int, layerStartIndex:int, layers: string list) = 
        if layerStartIndex >= input.Length
        then
            layers
        else
            let nextLayer = input.Substring(layerStartIndex, layerSize)
            splitLayers(input, layerSize, layerStartIndex + layerSize, layers @ [nextLayer])

    let digitCount (input:string) = input |> Seq.countBy id |> Seq.toList

    let findDigitTuple(tupleList: (char * int) list, charToMatch: char) = 
        List.find (fun (x, y) -> x = charToMatch) tupleList

[<EntryPoint>]
let main argv =
    let input = Day8Solution.readFileLine argv.[0]
    let width = 25
    let height = 6    
    let layers = Day8Solution.splitLayers(input, width * height, 0, List.empty)
    
    let digitCounts = List.map(fun elem -> Day8Solution.digitCount(elem)) layers
    
    let minimumZeroTuple = digitCounts |> List.minBy (fun elem -> 
        let (c, i) = Day8Solution.findDigitTuple(elem, '0')
        i)

    let result = 
        let (c1, v1) = Day8Solution.findDigitTuple(minimumZeroTuple, '1')
        let (c2, v2) = Day8Solution.findDigitTuple(minimumZeroTuple, '2')
        v1 * v2

    printfn "%i" result    

    0 // return an integer exit code
