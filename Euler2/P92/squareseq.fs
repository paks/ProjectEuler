///Project Euler Problem 92
///A number chain is created by continuously adding the square of the digits in a number to form 
///a new number until it has been seen before.
///
///For example,
///
///         44 -> 32 -> 13 -> 10 -> (1) -> (1)
///         85 -> (89) -> 145 -> 42 -> 20 -> 4 -> 16 -> 37 -> 58 -> (89)
///
///Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most 
///amazing is that EVERY starting number will eventually arrive at 1 or 89.
///
///How many starting numbers below ten million will arrive at 89?
//-----------------------------------------------------------------------
// <copyright file="squareseq.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic

let sqSum (n :int) = n.ToString().ToCharArray() |> Seq.map(fun c -> int c - int '0') |> Seq.sumBy(fun n -> n * n)

let isSeq89 = 
    let cache = Array.create 568 (-1)
    fun n -> 
        let rec isSeq89' n =
            let next = sqSum n
            if cache.[next] <> -1 then
                cache.[next]
            else
                match next with
                    | 1  -> 0
                    | 89 -> 1
                    | _  -> isSeq89' next
        let result = isSeq89' n
        if n <= 567 then
            cache.[n] <- result
        result

let solution = seq { 1 .. 9999999 } |> Seq.map(isSeq89) |> Seq.sum
solution |> printfn "solution: %d"

Console.ReadKey(true) |> ignore