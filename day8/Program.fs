open System
open System.IO

module Day8Solution = 
    exception InvalidLayeringException of obj

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

    let solveProblem1(input:string, imageWidth: int, imageHeight: int) = 
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

    let rec resolvePixel(layerPixels: char list) = 
        match layerPixels with
        | [] -> raise(InvalidLayeringException())
        | xs :: tail -> 
            match xs with
            | '2' -> resolvePixel tail
            | _ -> xs.ToString()

    let rec resolveLayers(layerLists: string list, currentPixelIndex: int, layerOutput: string list, layerRowLength: int) = 
        match currentPixelIndex with
        | index when index = layerRowLength -> layerOutput
        | _ -> 
            let nextPixel = List.map (fun (x:string) -> x.[currentPixelIndex]) layerLists
            let resolvedPixel = resolvePixel nextPixel
            resolveLayers(layerLists, currentPixelIndex + 1, layerOutput @ [resolvedPixel], layerRowLength)

    let solveProblem2(input:string, imageWidth: int, imageHeight: int) = 
        let layers = (splitLayers(input, imageWidth * imageHeight, 0, List.empty))
        let updatedLayers = resolveLayers(layers, 0, List.empty, imageWidth * imageHeight)
        splitLayers(updatedLayers |> String.concat "", imageWidth, 0, List.empty)
        
[<EntryPoint>]
let main argv =
    let input = Day8Solution.readFileLine argv.[0]
    let imageWidth = argv.[1] |> int
    let imageHeight = argv.[2] |> int
    
    let result = Day8Solution.solveProblem2(input, imageWidth, imageHeight)
    
    for pixelRow in result do
        printfn "%s" pixelRow
        
    0 // return an integer exit code
