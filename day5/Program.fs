open System.IO

module Day5Solution =
    exception InvalidIntCodeSequenceException of int * int
    exception InvalidParameterModeException of int

    // this solution expects all input to be on a single line
    let readFileLine (filePath:string) = Seq.toList(File.ReadLines(filePath)).[0]

    let parseStringToIntArray (str:string) = str.Split ',' |> Array.map (fun x -> x |> int) |> Seq.toList

    let retrieveParameter(instructions: int list, currentInstruction: int, instructionOffset: int, parameterMode: int) = 
        match parameterMode with 
        | 0 -> instructions.[instructions.[currentInstruction + instructionOffset]]
        | 1 -> instructions.[currentInstruction + instructionOffset]
        | _ -> raise(InvalidParameterModeException(parameterMode))

    let rec parseParameterModes(opCodeParameters: string, parameterModeArray: int list) = 
        // TODO: this could be a bit smarter
        if System.String.IsNullOrEmpty(opCodeParameters)
        then
            if parameterModeArray.Length = 1
            then
                List.concat [parameterModeArray; [0]]
            else
                parameterModeArray
        else
            let nextParameterMode = opCodeParameters.[opCodeParameters.Length - 1].ToString() |> int
            parseParameterModes(opCodeParameters.Substring(0, opCodeParameters.Length - 1), List.concat [parameterModeArray; [nextParameterMode]])

    let performAddition (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int) = 
        retrieveParameter(instructions, currentInstruction, 1, parameter1Mode) + retrieveParameter(instructions, currentInstruction, 2, parameter2Mode)

    let performMultiplication (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int) =
        retrieveParameter(instructions, currentInstruction, 1, parameter1Mode) * retrieveParameter(instructions, currentInstruction, 2, parameter2Mode)

    let performJumpIfTrueOperation (instructions: int list, currentInstruction: int, parameter1Mode: int) = 
        retrieveParameter(instructions, currentInstruction, 1, parameter1Mode) <> 0

    let performLessThanOperation (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int) = 
        retrieveParameter(instructions, currentInstruction, 1, parameter1Mode) < retrieveParameter(instructions, currentInstruction, 2, parameter2Mode)

    let performEqualToOperation (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int) = 
        retrieveParameter(instructions, currentInstruction, 1, parameter1Mode) = retrieveParameter(instructions, currentInstruction, 2, parameter2Mode)

    let rec intCodeProcessor (instructions: int list, currentInstruction: int, input: int) = 
        let originalOpCode = instructions.[currentInstruction]
        let stringOpCode = originalOpCode.ToString()
        let opCode = 
            if originalOpCode > 99
            then
                stringOpCode.Substring(stringOpCode.Length - 2) |> int
            else
                originalOpCode

        let parameterModeList = 
            if originalOpCode > 99
            then
                parseParameterModes(stringOpCode.Substring(0, stringOpCode.Length - 2), List.Empty)
            else
                [0; 0]

        let parameter1Mode = parameterModeList.[0]
        let parameter2Mode = parameterModeList.[1]
        
        // printfn "Current instruction set: %A" instructions
        // printfn "OpCode: %s" stringOpCode

        if opCode = 99 
        then 
            instructions
        elif opCode = 3
        then
            let targetIndex = instructions.[currentInstruction + 1]
            let updatedInstructions = List.concat [instructions.[..targetIndex - 1]; [input]; instructions.[targetIndex + 1..]]
            intCodeProcessor(updatedInstructions, currentInstruction + 2, input)
        elif opCode = 4
        then
            let valueToPrint = retrieveParameter(instructions, currentInstruction, 1, parameter1Mode)
            printfn "%i" valueToPrint
            intCodeProcessor(instructions, currentInstruction + 2, input)
        elif opCode = 5
        then
            if performJumpIfTrueOperation(instructions, currentInstruction, parameter1Mode)
            then
                let newInstructionPointer = retrieveParameter(instructions, currentInstruction, 2, parameter2Mode)
                intCodeProcessor(instructions, newInstructionPointer, input)
            else
                intCodeProcessor(instructions, currentInstruction + 3, input)
        elif opCode = 6
        then
            if not(performJumpIfTrueOperation(instructions, currentInstruction, parameter1Mode))
            then
                let newInstructionPointer = retrieveParameter(instructions, currentInstruction, 2, parameter2Mode)
                intCodeProcessor(instructions, newInstructionPointer, input)
            else
                intCodeProcessor(instructions, currentInstruction + 3, input)
        elif opCode = 7
        then
            let valueToWrite = 
                if performLessThanOperation(instructions, currentInstruction, parameter1Mode, parameter2Mode)
                then
                    1
                else
                    0
            let targetIndex = instructions.[currentInstruction + 3]
            let updatedInstructions = List.concat [instructions.[..targetIndex - 1]; [valueToWrite]; instructions.[targetIndex + 1..]]
            intCodeProcessor(updatedInstructions, currentInstruction + 4, input)
        elif opCode = 8
        then
            let valueToWrite = 
                if performEqualToOperation(instructions, currentInstruction, parameter1Mode, parameter2Mode)
                then
                    1
                else
                    0
            let targetIndex = instructions.[currentInstruction + 3]
            let updatedInstructions = List.concat [instructions.[..targetIndex - 1]; [valueToWrite]; instructions.[targetIndex + 1..]]
            intCodeProcessor(updatedInstructions, currentInstruction + 4, input)
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
    let inputFileName = argv.[0]
    let input = argv.[1] |> int
    let instructions = Day5Solution.parseStringToIntArray (Day5Solution.readFileLine inputFileName)
    let resultArray = Day5Solution.intCodeProcessor (instructions, 0, input)
    // printfn "Final instruction set: %A" resultArray
    0 // return an integer exit code