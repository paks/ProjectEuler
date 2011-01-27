///Project Euler Problem 58
///Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 
///7 is formed.
///
///                                      (37)36 35 34 33 32 (31)
///                                      38 (17)16 15 14 (13)30
///                                      39  18 (5) 4 (3) 12 29
///                                      40  19  6  1  2  11 28
///                                      41  20 (7) 8  9  10 27
///                                      42  21 22 23 24  25 26
///                                      (43)44 45 46 47  48 49
///
///It is interesting to note that the odd squares lie along the bottom right diagonal, but what is 
///more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a 
///ratio of 8/13 62%.
///
///If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 
///will be formed. If this process is continued, what is the side length of the square spiral for 
///which the ratio of primes along both diagonals first falls below 10%?
//-----------------------------------------------------------------------
// <copyright file="diagonal.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Numerics
open Euler2

let limit = 15000

let diagonalsSeq = 
    let diag n = 
        let n2 = n*n
        let n1 = (n - 1)
        [n2;n2-n1;n2 - (2*n1);n2 - (3*n1)]
    seq {
        let next = ref 3
        while true do
            yield diag (!next)
            do next := !next + 2
    }

let diagonals = diagonalsSeq |> Seq.take limit |> Array.ofSeq        

let numPrimes = 
    let toBigint (n :int) = bigint n
    // isPrime uses the Miller–Rabin primality test defined on the module Euler2
    Seq.sumBy(toBigint >> isPrime >> (fun b -> if b then 1 else 0))
    
let solution = 
    seq {0 .. limit - 1 } 
    |> Seq.scan(fun (order, (totalPrimes,size)) n -> 
                        let order = (2*n + 3)
                        let size = (4*(n+1)+1)
                        order, (totalPrimes + (diagonals.[n] |> numPrimes), size)) (0,(0,0)) 
    |> Seq.find(fun (_, (totalPrimes,size)) -> totalPrimes*10 < size) 
    |> fst

solution |> printfn "solution: %d"

Console.ReadKey(true) |> ignore
