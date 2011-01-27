///Project Euler Problem 97
///The first known prime found to exceed one million digits was discovered in 1999, and is a 
///Mersenne prime of the form 269725931; it contains exactly 2,098,960 digits. Subsequently other 
///Mersenne primes, of the form 2p1, have been found which contain more digits.
///
///However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 
///digits: 28433x2^7830457+1.
///
///Find the last ten digits of this prime number.
//-----------------------------------------------------------------------
// <copyright file="mersenne.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open Euler2
open System.Diagnostics
let sw = new Stopwatch()
sw.Start()

let exp = 7830457I
let num = 28433I

let e = powMod 10000000000I 2I exp
let result = ((num * e) + 1I) % 10000000000I
result |> printfn "solution: %A" 
sw.Stop()
sw.Elapsed |> printfn "%A"

Console.ReadKey(true) |> ignore
