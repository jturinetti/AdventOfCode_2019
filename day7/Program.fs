open System.IO

module Day7Solution =
    exception InvalidIntCodeSequenceException of int * int
    exception InvalidParameterModeException of int

    let mutable amplifierOutput = -1
    let mutable halted = false
    let mutable currentInstructions = []
    let mutable currentInstructionIndex = -1

    // this solution expects all input to be on a single line
    let readFileLine (filePath:string) = Seq.toList(File.ReadLines(filePath)).[0]

    let parseStringToIntArray (str:string) = str.Split ',' |> Array.map (fun x -> x |> int) |> Seq.toList

    // these permutation functions are borrowed from StackOverflow https://stackoverflow.com/questions/1526046/f-permutations
    let distrib e L =
        let rec aux pre post = 
            seq {
                match post with
                | [] -> yield (L @ [e])
                | h::t -> yield (List.rev pre @ [e] @ post)
                          yield! aux (h::pre) t 
            }
        aux [] L

    let rec perms = function 
        | [] -> Seq.singleton []
        | h::t -> Seq.collect (distrib h) (perms t)

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
        | 99 -> 
            halted <- true
            instructions
        | 1 -> mathFunc(add, instructions, currentInstruction, parameter1Mode, parameter2Mode, input)
        | 2 -> mathFunc(multiply, instructions, currentInstruction, parameter1Mode, parameter2Mode, input)
        | 3 ->        
            if input.IsEmpty
            then
                instructions
            else            
                let targetIndex = instructions.[currentInstruction + 1]
                let updatedInstructions = List.concat [instructions.[..targetIndex - 1]; [input.Head]; instructions.[targetIndex + 1..]]
                intCodeProcessor(updatedInstructions, currentInstruction + 2, input.[1..])
        | 4 ->            
            let valueToPrint = getValueByParameterMode(instructions, currentInstruction, 1, parameter1Mode)
            printfn "%i" valueToPrint
            amplifierOutput <- valueToPrint // set mutable output value
            currentInstructions <- instructions // set mutable instructions array
            currentInstructionIndex <- currentInstruction + 2   // set mutable instruction pointer
            instructions
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

    and amplificationCircuit (instructions: int list, phaseSettingArray: int list, phaseIndex: int, input: int) = 
        if phaseIndex = phaseSettingArray.Length
        then
            amplifierOutput
        else
            let phaseSetting = phaseSettingArray.[phaseIndex].ToString() |> int
            intCodeProcessor (instructions, 0, [phaseSetting; input]) |> ignore
            amplificationCircuit (instructions, phaseSettingArray, phaseIndex + 1, amplifierOutput)

    and amplificationCircuitWithFeedbackLoop (instructionsArray: int list list, currentInstructionArray: int list, haltedArray: bool list, phaseSettingArray: int list, phaseIndex: int, input: int) = 
        halted <- false
        if List.forall(fun elem -> elem = true) haltedArray
        then
            printfn "All amplifiers have halted!"
            amplifierOutput
        else
            let inputArray = 
                if phaseIndex >= phaseSettingArray.Length
                then
                    [input]
                else
                    let phaseSetting = phaseSettingArray.[phaseIndex].ToString() |> int
                    [phaseSetting; input]
            
            let modIndex = phaseIndex % phaseSettingArray.Length
            let updatedInstructions = intCodeProcessor (instructionsArray.[modIndex], currentInstructionArray.[modIndex], inputArray)
            let updatedInstructionsList = List.concat [instructionsArray.[..modIndex - 1]; [updatedInstructions]; instructionsArray.[modIndex + 1..]]
            let updatedInstructionPointerList = List.concat [currentInstructionArray.[..modIndex - 1]; [currentInstructionIndex]; currentInstructionArray.[modIndex + 1..]]
            let updatedHaltedList = List.concat [haltedArray.[..modIndex - 1]; [halted]; haltedArray.[modIndex + 1..]]
            
            amplificationCircuitWithFeedbackLoop (updatedInstructionsList, updatedInstructionPointerList, updatedHaltedList, phaseSettingArray, phaseIndex + 1, amplifierOutput)           

    and findLargestSignalOutput (instructions: int list) = 
        let phaseSettingArray = [0; 1; 2; 3; 4;]
        let mutable maximumSignalOutput = -1
        for phaseSettingPermutation in (perms phaseSettingArray) do
            let signalOutput = amplificationCircuit (instructions, phaseSettingPermutation, 0, 0)
            if signalOutput > maximumSignalOutput
            then
                printfn "New maximum found (%i) for phase setting %A" signalOutput phaseSettingPermutation
                maximumSignalOutput <- signalOutput

        maximumSignalOutput

    and findLargestSignalOutputWithFeedbackLoop (instructions: int list) = 
        let phaseSettingArray = [5; 6; 7; 8; 9;]
        let instructionsList = [instructions; instructions; instructions; instructions; instructions]
        let mutable maximumSignalOutput = -1
        for phaseSettingPermutation in (perms phaseSettingArray) do
            printfn "Phase Permutation: %A" phaseSettingPermutation
            resetMutableVariables()
            let signalOutput = amplificationCircuitWithFeedbackLoop (instructionsList, [0; 0; 0; 0; 0], [false; false; false; false; false], phaseSettingPermutation, 0, 0)
            if signalOutput > maximumSignalOutput
            then
                printfn "New maximum found (%i) for phase setting %A" signalOutput phaseSettingPermutation
                maximumSignalOutput <- signalOutput

        maximumSignalOutput

    and mapIndexToAmplifier (index: int) = 
        match index with
        | 0 -> "A"
        | 1 -> "B"
        | 2 -> "C"
        | 3 -> "D"
        | 4 -> "E"
        | _ -> "Unknown amplifier!"

    and resetMutableVariables() = 
        amplifierOutput <- -1
        halted <- false
        currentInstructions <- []
        currentInstructionIndex <- -1

[<EntryPoint>]
let main argv =
    let inputFileName = argv.[0]
    let instructions = Day7Solution.parseStringToIntArray (Day7Solution.readFileLine inputFileName)
    
    // let phaseSettingString = argv.[1].ToString()
    // let phaseSettingArray = phaseSettingString |> Array.ofSeq |> Array.map(fun c -> (c.ToString() |> int))
    // let outputSignal = Day7Solution.amplificationCircuit (instructions, phaseSettingArray, 0, 0)
    // printfn "Output Signal for phase setting %s is %i" phaseSettingString outputSignal

    // let largestOutputSignal = Day7Solution.findLargestSignalOutput (instructions)
    // printfn "Largest Output Signal is %i" largestOutputSignal

    // let phaseSettingString = argv.[1].ToString()
    // let phaseSettingArray = phaseSettingString |> Array.ofSeq |> Array.map(fun c -> (c.ToString() |> int))
    // let instructionsList = [instructions; instructions; instructions; instructions; instructions]
    // let outputSignal = Day7Solution.amplificationCircuitWithFeedbackLoop(instructionsList, [0; 0; 0; 0; 0], [false; false; false; false; false], phaseSettingArray, 0, 0)
    // printfn "Output Signal for phase setting with feedback loop %s is %i" phaseSettingString outputSignal

    let largestOutputSignal = Day7Solution.findLargestSignalOutputWithFeedbackLoop (instructions)
    printfn "Largest Output Signal is %i" largestOutputSignal

    0 // return an integer exit code