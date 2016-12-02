#!/usr/local/bin/fsharpi
open System
open System.IO

let input = 
    File.ReadAllText "./input.txt" 
    |> (fun (s:string) -> s.Split([|","|], StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun i -> i.Trim())

type Turn = 
    | Right
    | Left

type BlocksForward = int

type Instruction = Turn * BlocksForward 

let parseInstruction (value:string) =
        let turn =
            match value.ToCharArray() |> Array.head with
                | 'R' -> Right
                | 'L' -> Left
                | _ -> failwith (sprintf "Expected to get R or L as first char in '%s'" value) 
        let forward =
            Convert.ToInt32(value.Substring(1))
        (turn, forward)
    
let instructions = 
    input
    |> Array.map parseInstruction

type Direction =
    | North
    | South
    | West
    | East

type Location = int * int
type Position = Location * Direction

let moveLeft direction =
    match direction with
        | North -> West
        | West -> South
        | South -> East
        | East -> North
let moveRight direction =
    match direction with
        | North -> East
        | West -> North
        | South -> West
        | East -> South

let move position instruction =
    let (x, y, direction) = position
    let (turn, forward) = instruction
    let newDirection =
        match turn with
            | Right -> moveRight direction
            | Left -> moveLeft direction
    match newDirection with 
        | North -> (x, y + forward, newDirection)
        | West -> (x - forward, y, newDirection)
        | South -> (x, y - forward, newDirection)
        | East -> (x + forward, y, newDirection)

let endPosition = instructions |> Array.fold move (0, 0, North)
let (x, y, _) = endPosition 
let distanceInBlocks  = Math.Abs x + Math.Abs y

printfn "Headquotes are %i blocks away." distanceInBlocks