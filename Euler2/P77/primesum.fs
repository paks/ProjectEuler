///Project Euler Problem 77
///It is possible to write ten as the sum of primes in exactly five different ways:
///
///                     7 + 3
///                     5 + 5
///                     5 + 3 + 2
///                     3 + 3 + 2 + 2
///                     2 + 2 + 2 + 2 + 2
///
///What is the first value which can be written as the sum of primes in over five thousand 
///different ways?
//-----------------------------------------------------------------------
// <copyright file="primesum.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open Euler2


///see http://mathworld.wolfram.com/EulerTransform.html equations (7,8) with a(n) = 0 for composite and a(n) = 1 for prime
///see http://www.research.att.com/~njas/sequences/A000607 
///a(n) = 1/n*Sum_{k=1..n} A008472(k)*a(n-k). - Vladeta Jovovic
let primes = primes |> Seq.map int

let c n = primes |> Seq.takeWhile(fun p -> p <= n) |> Seq.sumBy(fun p -> if n % p = 0 then p else 0)

#nowarn "40"
let rec a = memoize(fun n ->
    match n with
        | _ when n < 0 -> 0
        | 0 -> 1
        | 1 -> 0
        | 2 -> 1
        | _ -> (seq { for k in 1 .. n -> (c k) * a (n - k) } |> Seq.sum) / n )

let aSeq = 1 |> Seq.unfold(fun n -> Some((n,a n),n+1))
let solution = aSeq |> Seq.find(fun (n,pn) -> pn >= 5000) |> fst
solution |> printfn "%d"
Console.ReadKey(true) |> ignore