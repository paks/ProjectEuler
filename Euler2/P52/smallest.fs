///Project Euler Problem 52
///It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, 
///but in a different order.
///
///Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
//-----------------------------------------------------------------------
// <copyright file="smalest.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System


let (==) a b = 
            if a.ToString().Length <> b.ToString().Length then
                false
            else
                let sort n = new String(n.ToString().ToCharArray() |> Array.sort)
                sort a = sort b

let numbers =
    seq {
        for i in 1 .. 1000000 do
            if i == 2*i && 2*i == 3*i && 3*i == 4*i && 4*i == 5*i && 5*i == 6*i then
                yield i
    }

numbers |> Seq.head |> printfn "solution: %d"
Console.ReadKey(true) |> ignore
