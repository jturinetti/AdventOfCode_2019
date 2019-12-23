open System.IO

module Day7Solution =
    exception InvalidIntCodeSequenceException of int * int
    exception InvalidParameterModeException of int

    let mutable amplifierOutput = -1

    // this solution expects all input to be on a single line
    let readFileLine (filePath:string) = Seq.toList(File.ReadLines(filePath)).[0]

    let parseStringToIntArray (str:string) = str.Split ',' |> Array.map (fun x -> x |> int) |> Seq.toList

    let getValueByParameterMode(instructions: int list, currentInstruction: int, instructionOffset: int, parameterMode: int) = 
        match parameterMode with 
        | 0 -> instructions.[instructions.[currentInstruction + instructionOffset]]
        | 1 -> instructions.[currentInstruction + instructionOffset]
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

    let add (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int) = 
        getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode) + getValueByParameterMode(instructions, currentInstruction, 2, parameter2Mode)

    let multiply (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int) =
        getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode) * getValueByParameterMode(instructions, currentInstruction, 2, parameter2Mode)

    let jumpIfTrue (instructions: int list, currentInstruction: int, parameter1Mode: int) = 
        getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode) <> 0

    let jumpIfFalse (instructions: int list, currentInstruction: int, parameter1Mode: int) = 
        not(jumpIfTrue(instructions, currentInstruction, parameter1Mode))

    let lessThan (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int) = 
        getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode) < getValueByParameterMode(instructions, currentInstruction, 2, parameter2Mode)

    let equalTo (instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int) = 
        getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode) = getValueByParameterMode(instructions, currentInstruction, 2, parameter2Mode)    

    let rec intCodeProcessor (instructions: int list, currentInstruction: int, input: int list) = 
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
        
        match opCode with
        | 99 -> instructions
        | 1 -> mathFunc(add, instructions, currentInstruction, parameter1Mode, parameter2Mode, input)
        | 2 -> mathFunc(multiply, instructions, currentInstruction, parameter1Mode, parameter2Mode, input)
        | 3 -> 
            let targetIndex = instructions.[currentInstruction + 1]
            let updatedInstructions = List.concat [instructions.[..targetIndex - 1]; [input.Head]; instructions.[targetIndex + 1..]]
            intCodeProcessor(updatedInstructions, currentInstruction + 2, input.[1..])
        | 4 ->
            let valueToPrint = getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode)
            printfn "%i" valueToPrint
            amplifierOutput <- valueToPrint
            intCodeProcessor(instructions, currentInstruction + 2, input)
        | 5 -> jumpFunc(jumpIfTrue, instructions, currentInstruction, parameter1Mode, parameter2Mode, input)
        | 6 -> jumpFunc(jumpIfFalse, instructions, currentInstruction, parameter1Mode, parameter2Mode, input)
        | 7 -> comparisonFunc(lessThan, instructions, currentInstruction, parameter1Mode, parameter2Mode, input)
        | 8 -> comparisonFunc(equalTo, instructions, currentInstruction, parameter1Mode, parameter2Mode, input)
        | _ -> raise (InvalidIntCodeSequenceException(instructions.[currentInstruction], currentInstruction))

    and mathFunc (func: (int list * int * int * int -> int), instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int, input: int list) = 
        let calculatedValue = func(instructions, currentInstruction, parameter1Mode, parameter2Mode)
        let targetIndex = instructions.[currentInstruction + 3]
        let updatedInstructions = List.concat [instructions.[..targetIndex - 1]; [calculatedValue]; instructions.[targetIndex + 1..]]
        intCodeProcessor(updatedInstructions, currentInstruction + 4, input)

    and jumpFunc (func: (int list * int * int -> bool), instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int, input: int list) = 
        if func(instructions, currentInstruction, parameter1Mode)
            then
                let newInstructionPointer = getValueByParameterMode(instructions, currentInstruction, 2, parameter2Mode)
                intCodeProcessor(instructions, newInstructionPointer, input)
            else
                intCodeProcessor(instructions, currentInstruction + 3, input)

    and comparisonFunc (func: (int list * int * int * int -> bool), instructions: int list, currentInstruction: int, parameter1Mode: int, parameter2Mode: int, input: int list) = 
        let valueToWrite = 
            if func(instructions, currentInstruction, parameter1Mode, parameter2Mode)
            then
                1
            else
                0
        let targetIndex = instructions.[currentInstruction + 3]
        let updatedInstructions = List.concat [instructions.[..targetIndex - 1]; [valueToWrite]; instructions.[targetIndex + 1..]]
        intCodeProcessor(updatedInstructions, currentInstruction + 4, input)

    and amplificationCircuit (instructions: int list, phaseSettingString: string, phaseIndex: int, input: int) = 
        if phaseIndex = phaseSettingString.Length
        then
            amplifierOutput
        else
            let phaseSetting = phaseSettingString.[phaseIndex].ToString() |> int
            intCodeProcessor (instructions, 0, [phaseSetting; input]) |> ignore
            amplificationCircuit (instructions, phaseSettingString, phaseIndex + 1, amplifierOutput)

[<EntryPoint>]
let main argv =
    let inputFileName = argv.[0]
    let instructions = Day7Solution.parseStringToIntArray (Day7Solution.readFileLine inputFileName)
    let phaseSettingString = argv.[1].ToString()
    let input = argv.[2] |> int

    let outputSignal = Day7Solution.amplificationCircuit (instructions, phaseSettingString, 0, input)

    printfn "Output Signal for phase setting %s is %i" phaseSettingString outputSignal

    0 // return an integer exit code