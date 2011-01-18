///Project Euler Problem 4
///A palindromic number reads the same both ways. 
///The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 * 99.
///
///Find the largest palindrome made from the product of two 3-digit numbers.
//-----------------------------------------------------------------------
// <copyright file="palin.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let toStr (x : int)  = x.ToString()
let rev   (x : string) = new String(Array.rev (x.ToCharArray()))

let palindrome =
    seq {
        for i in 100 .. 999 do
             for j in 100 .. 999 do
                let number = toStr (i * j)
                let reverse = rev number
                if number = reverse then
                    yield (i * j, i.ToString() + "," + j.ToString())
    } 
 
palindrome |> Seq.maxBy fst |> fst |> printfn "solution: %d"

Console.ReadKey(true) |> ignore