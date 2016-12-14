#!/usr/local/bin/fsharpi
open System
open System.IO

type Room = {
    EncriptedName : string;
    SectorId : int;
    Checksum: string }

let instructions = 
    File.ReadAllLines (__SOURCE_DIRECTORY__ + "/input.txt") 
    |> Array.toList

