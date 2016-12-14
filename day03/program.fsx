#!/usr/local/bin/fsharpi
open System
open System.IO

type Triangle = int * int * int

let instructions : Triangle list = 
    File.ReadAllLines (__SOURCE_DIRECTORY__ + "/input.txt") 
    |> Array.mapi (fun i (s:string)  -> 
        match s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries) with 
        | [|a; b; c|] -> (Convert.ToInt32(a), Convert.ToInt32(b), Convert.ToInt32(c))
        | _ -> failwithf "Invalid instruction at line %i" i) 
    |> Array.toList

let isValid (triangle:Triangle) = 
    let (a, b, c) = triangle
    let sorted = [|a; b; c|] |> Array.sort
    sorted.[0] + sorted.[1] > sorted.[2]  

let validTriangles =
    instructions 
    |> List.filter isValid    

printfn "There are %i possibe triangles" (List.length validTriangles) // 983

let verticalTringles =
    [0 .. 3 .. (List.length instructions - 1)]
    |> List.collect (fun i ->
        let (a1, b1, c1) = instructions.[i] 
        let (a2, b2, c2) = instructions.[i + 1] 
        let (a3, b3, c3) = instructions.[i + 2] 
        [(a1, a2, a3); (b1, b2, b3); (c1, c2, c3)])

printfn "And if you look vertically, there are %i possibe triangles" (verticalTringles |> List.filter isValid |> List.length) // 1836

