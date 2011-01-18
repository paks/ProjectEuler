///Project Euler Problem 3
///The prime factors of 13195 are 5, 7, 13 and 29.
///
///What is the largest prime factor of the number 600851475143 ?
//-----------------------------------------------------------------------
// <copyright file="prime.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open Euler

let number = 600851475143UL
let max = uint64 (Math.Sqrt(float (number + 1UL)))

let factors =
   seq {
        for i in 3UL .. 2UL ..  max do
            if number % i = 0UL  then
                yield i
   }
   
let isPrime = 
    let primesSet = primes |> Seq.takeWhile(fun p -> p < max) |> Set.ofSeq
    fun n -> primesSet.Contains(n)

let biggestPrime = factors |> Seq.filter isPrime |> Seq.max

printfn "solution: %d" biggestPrime
Console.ReadKey(true) |> ignore
