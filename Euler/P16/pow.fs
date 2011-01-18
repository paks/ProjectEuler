///Project Euler Problem 16
///2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
///
///What is the sum of the digits of the number 2^1000?
//-----------------------------------------------------------------------
// <copyright file="pow.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

(pown 2I 1000).ToString().ToCharArray() 
    |> Array.map(fun c -> int c - int '0') 
    |> Array.sum 
    |> printfn "solution: %d"
Console.ReadKey(true) |> ignore