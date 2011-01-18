///Project Euler Problem 47
///The first two consecutive numbers to have two distinct prime factors are:
///
///14 = 2 7
///15 = 3 5
///
///The first three consecutive numbers to have three distinct prime factors are:
///
///644 = 2² 7 23
///645 = 3 5 43
///646 = 2 17 19.
///
///Find the first four consecutive integers to have four distinct primes factors. What is the first of 
///these numbers?
//-----------------------------------------------------------------------
// <copyright file="distfactors.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open Euler


let isSolution a = seq { a .. a+3 } |> Seq.map float |> Seq.forall(primeFactors >> Seq.length >> (=) 4)

let solution = seq { 1 .. 1000000 } |> Seq.find isSolution
solution |> printfn "solution: %d"
        
Console.ReadKey(true) |> ignore
