///Project Euler Problem 1
///If we list all the natural numbers below 10 that 
///are multiples of 3 or 5, we get 3, 5, 6 and 9. 
///The sum of these multiples is 23.
///Find the sum of all the multiples of 3 or 5 below 1000.
//-----------------------------------------------------------------------
// <copyright file="mod35.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let problem1 =
    seq {1 .. 999}
    |> Seq.filter (fun x -> x % 3 = 0 || x % 5 = 0)
    |> Seq.sum

printfn "solution: %d" problem1
Console.ReadKey(true) |> ignore