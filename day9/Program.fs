open System.IO

module Day9Solution =
    exception InvalidIntCodeSequenceException of int * int
    exception InvalidParameterModeException of int

    let mutable debugMode = false

    // this solution expects all input to be on a single line
    let readFileLine (filePath:string) = Seq.toList(File.ReadLines(filePath)).[0]

    let parseStringToIntArray (str:string) = str.Split ',' |> Array.map (fun x -> x |> int) |> Seq.toList

    let getValueByParameterMode(instructions: int list, currentInstruction: int, instructionOffset: int, parameterMode: int, relativeBase: int) = 
        match parameterMode with 
        | 0 -> instructions.[instructions.[currentInstruction + instructionOffset]]
        | 1 -> instructions.[currentInstruction + instructionOffset]
        | 2 -> instructions.[currentInstruction + instructionOffset + relativeBase]
        | _ -> raise(InvalidParameterModeException(parameterMode))

    // not especially proud of this function but it works
    let rec parseParameterModes(opCodeParameters: string, parameterModeArray: int list) = 
        if parameterModeArray.Length = 2
        then
            parameterModeArray
        elif System.String.IsNullOrEmpty(opCodeParameters)
        then
            parseParameterModes(opCodeParameters, List.concat [parameterModeArray; [0]])
        else
            let nextParameterMode = opCodeParameters.[opCodeParameters.Length - 1].ToString() |> int
            parseParameterModes(opCodeParameters.Substring(0, opCodeParameters.Length - 1), List.concat [parameterModeArray; [nextParameterMode]])

    let add (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int, relativeBase: int) = 
        getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode, relativeBase) + getValueByParameterMode(instructions, currentInstruction, 2, parameter2Mode, relativeBase)

    let multiply (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int, relativeBase: int) =
        getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode, relativeBase) * getValueByParameterMode(instructions, currentInstruction, 2, parameter2Mode, relativeBase)

    let jumpIfTrue (instructions: int list, currentInstruction: int, parameter1Mode: int, relativeBase: int) = 
        getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode, relativeBase) <> 0

    let jumpIfFalse (instructions: int list, currentInstruction: int, parameter1Mode: int, relativeBase: int) = 
        not(jumpIfTrue(instructions, currentInstruction, parameter1Mode, relativeBase))

    let lessThan (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int, relativeBase: int) = 
        getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode, relativeBase) < getValueByParameterMode(instructions, currentInstruction, 2, parameter2Mode, relativeBase)

    let equalTo (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int, relativeBase: int) = 
        getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode, relativeBase) = getValueByParameterMode(instructions, currentInstruction, 2, parameter2Mode, relativeBase)    

    let rec intCodeProcessor (instructions: int list, currentInstruction: int, input: int, relativeBase: int) = 
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
                parseParameterModes("", List.Empty)

        let parameter1Mode = parameterModeList.[0]
        let parameter2Mode = parameterModeList.[1]

        if debugMode
        then
            printfn "OpCode %i" originalOpCode
            printfn "Parameter Modes %A" parameterModeList
        
        match opCode with
        | 99 -> instructions
        | 1 -> mathFunc(add, instructions, currentInstruction, parameter1Mode, parameter2Mode, input, relativeBase)
        | 2 -> mathFunc(multiply, instructions, currentInstruction, parameter1Mode, parameter2Mode, input, relativeBase)
        | 3 ->
            let updatedInstructions = 
                let targetIndex = instructions.[currentInstruction + 1]
                List.concat [instructions.[..targetIndex - 1]; [input]; instructions.[targetIndex + 1..]]
            intCodeProcessor(updatedInstructions, currentInstruction + 2, input, relativeBase)
        | 4 ->
            printfn "%i" (getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode, relativeBase))
            intCodeProcessor(instructions, currentInstruction + 2, input, relativeBase)
        | 5 -> jumpFunc(jumpIfTrue, instructions, currentInstruction, parameter1Mode, parameter2Mode, input, relativeBase)
        | 6 -> jumpFunc(jumpIfFalse, instructions, currentInstruction, parameter1Mode, parameter2Mode, input, relativeBase)
        | 7 -> comparisonFunc(lessThan, instructions, currentInstruction, parameter1Mode, parameter2Mode, input, relativeBase)
        | 8 -> comparisonFunc(equalTo, instructions, currentInstruction, parameter1Mode, parameter2Mode, input, relativeBase)
        | 9 -> intCodeProcessor(instructions, currentInstruction + 2, input, relativeBase + getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode, relativeBase))
        | _ -> raise (InvalidIntCodeSequenceException(instructions.[currentInstruction], currentInstruction))

    and mathFunc (func: (int list * int * int * int * int -> int), instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int, input: int, relativeBase: int) = 
        let updatedInstructions = 
            let calculatedValue = func(instructions, currentInstruction, parameter1Mode, parameter2Mode, relativeBase)
            let targetIndex = instructions.[currentInstruction + 3]
            List.concat [instructions.[..targetIndex - 1]; [calculatedValue]; instructions.[targetIndex + 1..]]
        intCodeProcessor(updatedInstructions, currentInstruction + 4, input, relativeBase)

    and jumpFunc (func: (int list * int * int * int -> bool), instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int, input: int, relativeBase: int) = 
        let funcResult = func(instructions, currentInstruction, parameter1Mode, relativeBase)
        match funcResult with
        | true -> 
            let newInstructionPointer = getValueByParameterMode(instructions, currentInstruction, 2, parameter2Mode, relativeBase)
            intCodeProcessor(instructions, newInstructionPointer, input, relativeBase)
        | false -> intCodeProcessor(instructions, currentInstruction + 3, input, relativeBase)

    and comparisonFunc (func: (int list * int * int * int * int -> bool), instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int, input: int, relativeBase: int) = 
        let valueToWrite = 
            let funcResult = func(instructions, currentInstruction, parameter1Mode, parameter2Mode, relativeBase)
            match funcResult with
            | true -> 1
            | false -> 0
        
        let updatedInstructions = 
            let targetIndex = instructions.[currentInstruction + 3]
            List.concat [instructions.[..targetIndex - 1]; [valueToWrite]; instructions.[targetIndex + 1..]]
        intCodeProcessor(updatedInstructions, currentInstruction + 4, input, relativeBase)

[<EntryPoint>]
let main argv =
    let inputFileName = argv.[0]
    let input = argv.[1] |> int
    Day9Solution.debugMode <- argv.Length > 2 && argv.[2] = "debug"
    let instructions = Day9Solution.parseStringToIntArray (Day9Solution.readFileLine inputFileName)
    Day9Solution.intCodeProcessor (instructions, 0, input, 0) |> ignore
    
    0 // return an integer exit code