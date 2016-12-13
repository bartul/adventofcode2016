#!/usr/local/bin/fsharpi
open System
open System.IO

let instructionsText = 
    File.ReadAllText (__SOURCE_DIRECTORY__ + "/input.txt") 
    |> (fun (s:string) -> s.Replace("\n", "P"))

type Instruction =
    | Up 
    | Down
    | Left
    | Right
    | PressKey

let parseInstruction value =
    match value with
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | 'P' -> PressKey
    | _ -> failwithf "Failed to parse instruction, encounted unrecognized one - '%c'" value


let ensurePressKeyOnEnd value = 
    if value |> Array.last <> 'P' then
        [|'P'|] |> Array.append value 
    else    
        value

let instructions = instructionsText.ToCharArray() |> ensurePressKeyOnEnd |> Array.toList |> List.map parseInstruction 

type Button = | B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8 | B9

// This is bollocks
let move currentButton instruction =
    match currentButton, instruction with 
    | B1, Right -> B2
    | B1, Down -> B4
    | B2, Left -> B1
    | B2, Right -> B3
    | B2, Down -> B5
    | B3, Left -> B2
    | B3, Down -> B6
    | B4, Up -> B1
    | B4, Right -> B5
    | B4, Down -> B7
    | B5, Up -> B2
    | B5, Left -> B4
    | B5, Down -> B8
    | B5, Right -> B6
    | B6, Up -> B3
    | B6, Left -> B5
    | B6, Down -> B9
    | B7, Up -> B4
    | B7, Right -> B8
    | B8, Left -> B7
    | B8, Up -> B5
    | B8, Right -> B9
    | B9, Up -> B6
    | B9, Left -> B8
    | button, _ -> button

let rec doit currentButton instructions code =
    match instructions with
    | [PressKey] ->
        currentButton::code
    | [_] | []->
        code
    | head::tail ->
        let nextButton = move currentButton (List.head instructions) 
        doit nextButton tail (if head = PressKey then currentButton::code else code)


let code = doit B5 instructions List.empty
printfn "Code is %A" (code |> List.rev)

type Button2 = | B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8 | B9 | BA | BB | BC | BD

// This is even bigger bollocks
let move2 currentButton instruction =
    match currentButton, instruction with 
    | B1, Down -> B3
    | B2, Right -> B3
    | B2, Down -> B6
    | B3, Up -> B1
    | B3, Right -> B4
    | B3, Down -> B7
    | B3, Left -> B2
    | B4, Left -> B3
    | B4, Down -> B8
    | B5, Right -> B6
    | B6, Up -> B2
    | B6, Right -> B7
    | B6, Down -> BA
    | B6, Left -> B5
    | B7, Up -> B3
    | B7, Left -> B6
    | B7, Down -> BB
    | B7, Right -> B8
    | B8, Up -> B4
    | B8, Left -> B7
    | B8, Down -> BC
    | B8, Right -> B9
    | B9, Left -> B8
    | BA, Up -> B6
    | BA, Right -> BB
    | BB, Up -> B7
    | BB, Left -> BA
    | BB, Right -> BC
    | BB, Down -> BD
    | BC, Up -> B8
    | BC, Left -> BB
    | BD, Up -> BB
    | button, _ -> button

let rec doit2 currentButton instructions code =
    match instructions with
    | [PressKey] ->
        currentButton::code
    | [_] | []->
        code
    | head::tail ->
        let nextButton = move2 currentButton (List.head instructions) 
        doit2 nextButton tail (if head = PressKey then currentButton::code else code)

let code2 = doit2 B5 instructions List.empty
printfn "On the second hand code is %A" (code2 |> List.rev)