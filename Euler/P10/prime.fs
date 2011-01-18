///Project Euler Problem 10
///The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
///
///Find the sum of all the primes below two million.
//-----------------------------------------------------------------------
// <copyright file="prime.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

open Euler

// Using prime generator defined in the module Euler
primes |> Seq.takeWhile(fun p -> p < 2000000UL ) |> Seq.sum |> printfn "solution: %d"
Console.ReadKey(true) |> ignore