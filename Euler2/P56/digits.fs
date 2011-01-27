///Project Euler Problem 97
///A googol (10^100) is a massive number: one followed by one-hundred zeros; 100100 is almost 
///unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the 
///digits in each number is only 1.
///
///Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?
//-----------------------------------------------------------------------
// <copyright file="digits.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Numerics


let powSeq = 
    seq {
        for a in 1I .. 99I do
            for b in 1 .. 99 do
                yield pown a b
    }

let sumBigits n = n.ToString().ToCharArray() |> Array.map(fun ch -> int ch - int '0') |> Seq.sum

let result = powSeq |> Seq.map sumBigits |> Seq.max
                                    
result |> printfn "%d"

Console.ReadKey(true) |> ignore