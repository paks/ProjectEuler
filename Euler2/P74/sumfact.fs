///Project Euler Problem 74
///The number 145 is well known for the property that the sum of the factorial of its digits is equal 
///to 145:
///
///           1! + 4! + 5! = 1 + 24 + 120 = 145
///
///Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 
///169; it turns out that there are only three such loops that exist:
///
///           169 -> 363601 -> 1454 -> 169
///           871 -> 45361 -> 871
///           872 -> 45362 -> 872
///
///It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For 
///example,
///
///           69 -> 363600 -> 1454 -> 169 -> 363601 (-> 1454)
///           78 -> 45360 -> 871 -> 45361 (-> 871)
///           540 -> 145 (-> 145)
///
///Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating 
///chain with a starting number below one million is sixty terms.
///
///How many chains, with a starting number below one million, contain exactly sixty non-repeating 
///terms?
//-----------------------------------------------------------------------
// <copyright file="sumfact.fs" >
// Copyright © 2011 Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open System.Numerics
open Euler2

let sumFact = 
    let table =
            [|
                yield 1L
                for n in 1I .. 9I do
                    yield int64 (factorial n) |]
    fun n -> n.ToString().ToCharArray() |> Array.map(fun c -> (int c) - (int '0') ) |> Array.sumBy(fun d -> table.[d])

let factSeqLength n =
    let table = new HashSet<_>()
    let rec compute x =
        let next = sumFact x
        if not (table.Contains(next)) then
            table.Add(next) |> ignore
            compute next
    table.Add(n) |> ignore
    compute n 
    table.Count

//factSeqLength 145L |> printfn "145: %d"
//factSeqLength 169L |> printfn "169: %d"
//factSeqLength 69L |> printfn "69: %d"

let sixtyTerms = seq { 1L .. 999999L } |> Seq.filter(fun n -> factSeqLength n = 60)

let solution = sixtyTerms |> Seq.length

solution |> printfn "solution: %d"
Console.ReadKey(true) |> ignore