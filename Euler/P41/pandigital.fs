///Project Euler Problem 41
///We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly 
///once. For example, 2143 is a 4-digit pandigital and is also prime.
///
///What is the largest n-digit pandigital prime that exists?
//-----------------------------------------------------------------------
// <copyright file="pandigital.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Diagnostics
open System.Collections.Generic

open Euler

let limit = 987654321UL
let str = "123456789"
let sort n = new String(n.ToString().ToCharArray() |> Array.sort)

let isPandigital = function
            | n when n < 10UL -> false
            | n              -> let length = n.ToString().Length
                                let test = str.Substring(0,length)
                                test = sort n
(*
isPandigital 3I |> printfn "%A"
isPandigital 12I |> printfn "%A"
isPandigital 124I |> printfn "%A"
isPandigital 123I |> printfn "%A"
isPandigital 987654321I |> printfn "%A"
*)
// The old sieve method is too slow. It's time for a real prime generator.
let pandigitalPrimes = primes |> Seq.takeWhile(fun p -> p < limit) |> Seq.filter isPandigital
   
let solution = pandigitalPrimes |> Seq.max

printfn "soultion: %d" solution
Console.ReadKey(true) |> ignore