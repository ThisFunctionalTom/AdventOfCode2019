let rec interpret pos (program: int array) =
    let calc f pos =
        program.[program.[pos+3]] <- f (program.[program.[pos+1]]) (program.[program.[pos+2]])
        
    match program.[pos] with
    | 1 ->
        calc (+) pos
        interpret  (pos+4) program
    | 2 ->
        calc (*) pos
        interpret (pos+4) program
    | 99 ->
        Some program
    | _ ->
        None

let parseArray (input: string) =
    input.Split(',')
    |> Array.map int

let runProgram = parseArray >> interpret 0

#I ".paket/load"
#load "main.group.fsx"

open Swensen.Unquote

let check input output =
    test <@ runProgram input = Some (parseArray output) @>

check 
    "1,9,10,3,2,3,11,0,99,30,40,50" 
    "3500,9,10,70,2,3,11,0,99,30,40,50"

check
    "1,0,0,0,99"
    "2,0,0,0,99"
    
check 
    "2,3,0,3,99"
    "2,3,0,6,99"

check
    "2,4,4,5,99,0"
    "2,4,4,5,99,9801"

check
    "1,1,1,4,99,5,6,0,99"
    "30,1,1,4,2,5,6,0,99"

let run input noun verb =
    let restore (program: int array) =
        program.[1] <- noun
        program.[2] <- verb
        program
    
    (Array.copy input)
    |> restore
    |> interpret 0
    |> Option.map (fun prog -> prog.[0])

let input =
    System.IO.File.ReadAllText("day2.input")
    |> parseArray

let result1 = run input 12 2

let expectedOutput = 19690720

let result2 =
    let picker (noun, verb) =
        if run (Array.copy input) noun verb = Some expectedOutput then
            Some(noun, verb)
        else
            None
        
    List.allPairs [0..input.Length-1] [0..input.Length-1]
    |> List.pick picker
    |> fun (noun, verb) -> noun*100+verb