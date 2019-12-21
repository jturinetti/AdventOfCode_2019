open System.IO

module Day5Solution =
    exception InvalidIntCodeSequenceException of int * int
    exception InvalidParameterModeException of int

    // this solution expects all input to be on a single line
    let readFileLine (filePath:string) = Seq.toList(File.ReadLines(filePath)).[0]

    let parseStringToIntArray (str:string) = str.Split ',' |> Array.map (fun x -> x |> int) |> Seq.toList

    let performAddition (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int) = 
        let parameter1 = 
            match parameter1Mode with
            | 0 -> instructions.[instructions.[currentInstruction + 1]]
            | 1 -> instructions.[currentInstruction + 1]
            | _ -> raise(InvalidParameterModeException(parameter1Mode))
        let parameter2 = 
            match parameter2Mode with
            | 0 -> instructions.[instructions.[currentInstruction + 2]]
            | 1 -> instructions.[currentInstruction + 2]
            | _ -> raise(InvalidParameterModeException(parameter2Mode))
        parameter1 + parameter2

    let performMultiplication (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int) =
        let parameter1 = 
            match parameter1Mode with
            | 0 -> instructions.[instructions.[currentInstruction + 1]]
            | 1 -> instructions.[currentInstruction + 1]
            | _ -> raise(InvalidParameterModeException(parameter1Mode))
        let parameter2 = 
            match parameter2Mode with
            | 0 -> instructions.[instructions.[currentInstruction + 2]]
            | 1 -> instructions.[currentInstruction + 2]
            | _ -> raise(InvalidParameterModeException(parameter2Mode))
        parameter1 * parameter2    

    let rec intCodeProcessor (instructions: int list, currentInstruction: int, input: int) = 
        
        let originalOpCode = instructions.[currentInstruction]

        let opCode = 
            if originalOpCode > 99
            then
                let stringOpCode = originalOpCode.ToString()
                stringOpCode.Substring(stringOpCode.Length - 2) |> int                
            else
                originalOpCode

        let parameter1Mode = 
            if originalOpCode > 99
            then
                originalOpCode.ToString().[originalOpCode.ToString().Length - 3].ToString() |> int
            else
                0

        let parameter2Mode = 
            if originalOpCode > 99 && originalOpCode.ToString().Length > 3
            then
                originalOpCode.ToString().[originalOpCode.ToString().Length - 4].ToString() |> int
            else
                0

        if opCode = 99 
        then 
            instructions
        elif opCode = 3
        then            
            let targetIndex = 
                match parameter1Mode with 
                | 0 -> instructions.[currentInstruction + 1]
                | 1 -> instructions.[instructions.[currentInstruction + 1]]
                | _ -> raise(InvalidParameterModeException(parameter1Mode))
            let updatedInstructions = List.concat [instructions.[..targetIndex - 1]; [input]; instructions.[targetIndex + 1..]]
            intCodeProcessor(updatedInstructions, currentInstruction + 2, input)
        elif opCode = 4
        then            
            let valueToPrint = 
                match parameter1Mode with 
                | 0 -> instructions.[instructions.[currentInstruction + 1]]
                | 1 -> instructions.[currentInstruction + 1]
                | _ -> raise(InvalidParameterModeException(parameter1Mode))
            printfn "%s" (valueToPrint.ToString())
            intCodeProcessor(instructions, currentInstruction + 2, input)
        else            
            let newValue = 
                match opCode with 
                | 1 -> performAddition(instructions, currentInstruction, parameter1Mode, parameter2Mode)
                | 2 -> performMultiplication(instructions, currentInstruction, parameter1Mode, parameter2Mode)                
                | _ -> raise (InvalidIntCodeSequenceException(instructions.[currentInstruction], currentInstruction))
            let targetIndex = instructions.[currentInstruction + 3]
            let updatedInstructions = List.concat [instructions.[..targetIndex - 1]; [newValue]; instructions.[targetIndex + 1..]]
            intCodeProcessor(updatedInstructions, currentInstruction + 4, input)

[<EntryPoint>]
let main argv =
    let inputFileName = "input.txt"
    let input = argv.[0] |> int
    let instructions = Day5Solution.parseStringToIntArray (Day5Solution.readFileLine inputFileName)
    let resultArray = Day5Solution.intCodeProcessor (instructions, 0, input)
    0 // return an integer exit code