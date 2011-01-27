///Project Euler Problem 95
///The proper divisors of a number are all the divisors excluding the number itself. For example, 
///the proper divisors of 28 are 1, 2, 4, 7, and 14. As the sum of these divisors is equal to 28, we 
///call it a perfect number.
///
///Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 
///284 is 220, forming a chain of two numbers. For this reason, 220 and 284 are called an amicable 
///pair.
///
///Perhaps less well known are longer chains. For example, starting with 12496, we form an 
///amicable chain of five numbers:
///
///                      12496 -> 14288 -> 15472 -> 14536 -> 14264 -> ( 12496 -> ...)
///
///Find the smallest member of the longest amicable chain with no element exceeding one million.
//-----------------------------------------------------------------------
// <copyright file="amicable.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open Euler2
open System.Diagnostics
let sw = new Stopwatch()
sw.Start()

// Two ways of computing the sum of divisors function
let sumDiv' =
    let sumDiv' n =
        let sqr = int64 (Math.Sqrt (float n))
        match n with
            | 0L | 1L -> 0L
            | _ -> let rec sum i acc = 
                      if i < sqr then
                          if n % i = 0L  then
                              sum (i+1L) (acc + (i + (n/i)))
                          else
                              sum (i+1L) acc
                      else
                          if n % sqr = 0L then
                              acc + sqr
                          else
                              acc
                   sum 2L 1L
    memoize(fun n -> sumDiv' n)

let sumDiv: int64 -> int64 = 
    memoize(fun n ->
        match n with 
            | 0L | 1L -> 0L
            | _ -> (n |> float |> primeFactors |> Seq.fold(fun acc (p,n) -> (acc*((pown (int64 p) (n+1)) - 1L))/((int64 p) - 1L)) 1L) - n)

let limit = 1000000L

let amicable n = 
    let chain = new HashSet<_>()
    chain.Add(n) |> ignore
    let rec amicable' n = 
        [|
            yield n
            let next = sumDiv n
            if next > limit then
                yield -1L
            else
                if not (chain.Contains next) then
                    chain.Add next |> ignore
                    yield! amicable' next
                else
                    yield next
        |]
    amicable' n

//let list = amicable 14316
//list.Length |> printfn "%d"
//list |> List.iter (printf "%d, ")

let amicables = 
    seq {
        let last arr = let last = (arr |> Array.length) - 1 in arr.[last]
        
        for n in 1L .. limit do
            let chain = amicable n
            let head = chain.[0]
            let lst = last chain
            if lst <> -1L && head = lst then
                yield chain
    }
let solution = amicables 
               |> Seq.map(fun ami -> (ami.[0], ami.Length)) 
               |> Seq.maxBy snd |> fst
solution |> printfn "solution: %d"
sw.Stop()
sw.Elapsed |> printfn "%A"

Console.ReadKey(true) |> ignore