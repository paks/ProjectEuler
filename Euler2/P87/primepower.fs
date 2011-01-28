///Project Euler Problem 87
///The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28. 
///In fact, there are exactly four numbers below fifty that can be expressed in such a way:
///
///                          28 = 2^2 + 2^3 + 2^4
///                          33 = 3^2 + 2^3 + 2^4
///                          49 = 5^2 + 2^3 + 2^4
///                          47 = 2^2 + 3^3 + 2^4
///
///How many numbers below fifty million can be expressed as the sum of a prime square, prime 
///cube, and prime fourth power?
//-----------------------------------------------------------------------
// <copyright file="primepower.fs" >
// Copyright Î÷Î÷ Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open Euler2

let limit = 50000000UL

let primes1 = primes |> Seq.takeWhile(fun p -> p <  7072UL) |> Seq.toList // limit^(1/2)
let primes2 = primes1 |> Seq.takeWhile(fun p -> p < 369UL) |> Seq.toList // limit^(1/3)
let primes3 = primes1 |> Seq.takeWhile(fun p -> p < 84UL) |> Seq.toList // limit^(1/4)

let solution = 
    let table = new HashSet<_>()
    for a in primes1 do 
        for b in primes2 do 
            for c in primes3 do
                let num = (a*a) + (b*b*b) + (c*c*c*c)
                if  num < limit then
                    table.Add(num) |> ignore
    table.Count
                    
solution |> printfn "solution: %d"

Console.ReadKey(true) |> ignore

