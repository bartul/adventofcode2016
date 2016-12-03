#!/usr/local/bin/fsharpi
open System
open System.IO

type Turn = 
    | Right
    | Left

type BlocksForward = int

type Instruction = Turn * BlocksForward 

type Direction =
    | North
    | South
    | West
    | East


type Position = int * int * Direction
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
    File.ReadAllText (__SOURCE_DIRECTORY__ + "/input.txt") 
    |> (fun (s:string) -> s.Split([|","|], StringSplitOptions.RemoveEmptyEntries))
    |> Array.map ((fun i -> i.Trim()) >> parseInstruction)



// Please replace with circular data structure
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

let (x, y, _) = instructions |> Array.fold move (0, 0, North)

let distanceInBlocks (x:int) (y:int) = Math.Abs x + Math.Abs y

printfn "Headquarters are %i blocks away, at (%i, %i)." (distanceInBlocks x y) x y

// For the love of god, there has to be a better solution than this one ?!
let traversePath x1 y1 x2 y2 =
    match 1 with
    | i when x1 = x2 && y1 <= y2 -> [y1..y2] |> List.map (fun i -> (x1, i))
    | i when x1 = x2 && y1 > y2 -> [y2..y1] |> List.map (fun i -> (x1, i)) |> List.rev
    | i when y1 = y2 && x1 <= x2 -> [x1..x2] |> List.map (fun i -> (i, y1))
    | i when y1 = y2 && x1 > x2 -> [x2..x1] |> List.map (fun i -> (i, y1)) |> List.rev
    | _ -> failwith "not supported path"
 
let rec getFirstVisitedTwice position memory instructions =
    let (x', y', _) = position 

    let newPosition =
        move position (Array.head instructions)

    let (x, y, _) = newPosition
    
    let path = List.tail (traversePath x' y' x y)
    let crossPath = path |> List.tryFind (fun i -> List.contains i memory)

    match crossPath with 
        | Some i -> 
            i
        | None ->
            getFirstVisitedTwice newPosition (List.concat [memory; path]) (Array.tail instructions) 

let (x2, y2) = getFirstVisitedTwice (0, 0, North) [(0, 0)] instructions

printfn "On other hand, Headquarters are %i blocks away, at (%i, %i)." (distanceInBlocks x2 y2) x2 y2



