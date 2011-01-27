///Project EuIer ProbIem 70
///Euler's Totient function, phi(n) [sometimes called the phi function], is used to determine the 
///number of positive numbers less than or equal to n which are relatively prime to n. For 
///example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, phi(9)=6.
///The number 1 is considered to be relatively prime to every positive number, so phi(1)=1.
///
///Interestingly, phi(87109)=79180, and it can be seen that 87109 is a permutation of 79180.
///
///Find the value of n, 1 n 10^7, for which phi(n) is a permutation of n and the ratio n/phi(n) 
///produces a minimum.
//-----------------------------------------------------------------------
// <copyright file="phinprime.fs" >
// Copyright © 2011 Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System
open Euler2

let sortDigits (n : int64) = 
    let arr = n.ToString().ToCharArray()
    let arr = arr |> Array.sort
    int (new String(arr))
let (==) p p1 = 
    sortDigits p = sortDigits p1

// I'm hopping here that the solution is between this primes
let primes = primes |> Seq.takeWhile(fun p -> p < 5000UL) |> Seq.map int64 |> List.ofSeq

let candidates = combinations primes 2 |> Seq.map(fun factors -> factors |> List.fold(*) 1L, factors) |> Seq.filter(fun (n,_) -> n < 10000000L)

/// http://en.wikipedia.org/wiki/Euler%27s_totient_function
let phi (n, factors) =
    let f n p = (n*p - n)/p
    (n, factors |> List.fold f n)


let solution = candidates
               |> Seq.map phi
               |> Seq.filter(fun (n,phin) -> n == phin)
               |> Seq.minBy(fun (n, phin) -> (float n)/(float phin)) 
               |> fst

solution |> printfn "solution: %d"

Console.ReadKey(true) |> ignore

