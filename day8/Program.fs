open System
open System.IO

module Day8Solution = 

    let imageWidth = 25
    let imageHeight = 6

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

    let solveProblem1(input:string) = 
        let layers = splitLayers(input, imageWidth * imageHeight, 0, List.empty)
    
        let digitCounts = List.map(fun elem -> digitCount(elem)) layers
        
        let minimumZeroTuple = digitCounts |> List.minBy (fun elem -> 
            let (c, i) = findDigitTuple(elem, '0')
            i)

        let result = 
            let (c1, v1) = findDigitTuple(minimumZeroTuple, '1')
            let (c2, v2) = findDigitTuple(minimumZeroTuple, '2')
            v1 * v2

        result

[<EntryPoint>]
let main argv =
    let input = Day8Solution.readFileLine argv.[0]
    
    printfn "%i" (Day8Solution.solveProblem1(input))

    0 // return an integer exit code
