open System.IO

module Day5Solution =
    exception InvalidIntCodeSequenceException of int * int

    // this solution expects all input to be on a single line
    let readFileLine (filePath:string) = Seq.toList(File.ReadLines(filePath)).[0]

    let parseStringToIntArray (str:string) = str.Split ',' |> Array.map (fun x -> x |> int) |> Seq.toList

    let performAddition (instructions: int list, currentInstruction: int) = 
        instructions.[instructions.[currentInstruction + 1]] + instructions.[instructions.[currentInstruction + 2]]

    let performMultiplication (instructions: int list, currentInstruction: int) = 
        instructions.[instructions.[currentInstruction + 1]] * instructions.[instructions.[currentInstruction + 2]]

    let rec intCodeProcessor (instructions: int list, currentInstruction: int) = 
        if instructions.[currentInstruction] = 99 
        then instructions
        else
            let newValue = 
                match instructions.[currentInstruction] with 
                | 1 -> performAddition(instructions, currentInstruction)
                | 2 -> performMultiplication(instructions, currentInstruction)
                | _ -> raise (InvalidIntCodeSequenceException(instructions.[currentInstruction], currentInstruction))
            let targetIndex = instructions.[currentInstruction + 3]
            let updatedInstructions = List.concat [instructions.[..targetIndex - 1]; [newValue]; instructions.[targetIndex + 1..]]
            intCodeProcessor(updatedInstructions, currentInstruction + 4)

[<EntryPoint>]
let main argv =
    let inputFileName = argv.[0]
    let input = Day5Solution.parseStringToIntArray (Day5Solution.readFileLine inputFileName)
    let resultArray = Day5Solution.intCodeProcessor (input, 0)
    printfn "First value in int code array: %s" (resultArray.[0].ToString())
    0 // return an integer exit code