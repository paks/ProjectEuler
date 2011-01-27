///Project Euler Problem 76
///It is possible to write five as a sum in exactly six different ways:
///
///                        4 + 1
///                        3 + 2
///                        3 + 1 + 1
///                        2 + 2 + 1
///                        2 + 1 + 1 + 1
///                        1 + 1 + 1 + 1 + 1
///
///How many different ways can one hundred be written as a sum of at least two positive integers?
//-----------------------------------------------------------------------
// <copyright file="sum.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open System.Numerics
open Euler2


///see http://mathworld.wolfram.com/PartitionFunctionP.html equation (11)
#nowarn "40"
let rec P = memoize(fun n ->
    match n with
        | _ when n < 0I -> 0I
        | _ when n = 0I -> 1I
        | _ when n = 1I -> 1I
        | _             -> Seq.sum [ for k in 1I .. n do
                                        let i = pown -1I (int (k+1I))
                                        let p1 = P(n - (k*(3I*k - 1I)/2I))
                                        let p2 = P(n - (k*(3I*k + 1I)/2I))
                                        yield i*(p1+p2) ])

let solution = P 100I - 1I
solution |> printfn "solution: %A"
Console.ReadKey(true) |> ignore
    