///Project Euler Problem 48
///The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
///
//Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
//-----------------------------------------------------------------------
// <copyright file="series.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Numerics


let result = [1I..1000I] |> List.mapi (fun i n -> pown  n (i + 1) ) |> List.sum

printfn "solution: %A" (result % 10000000000I)

Console.ReadKey(true) |> ignore
