///Project Euler Problem 7
///By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
///we can see that the 6th prime is 13.
///
///What is the 10001st prime number?
//-----------------------------------------------------------------------
// <copyright file="prime.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open Euler

primes |> Seq.nth 10000 |> printfn "solution: %d"

Console.ReadKey(true) |> ignore