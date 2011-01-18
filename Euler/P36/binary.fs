///Project Euler Problem 36
///The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
///
///Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
///
///(Please note that the palindromic number, in either base, may not include leading zeros.)
//-----------------------------------------------------------------------
// <copyright file="binary.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let rev   (x : string) = new String(Array.rev (x.ToCharArray()))

let isPalindrome n = 
        let num = n.ToString()
        num = rev num
        
let isBinPalindrome (n :int) =
        let bin = Convert.ToString(n,2)
        bin = rev bin
        
//printfn "%A" (isPalindrome 585)
//printfn "%A" (isBinPalindrome 585)

let numbers =
    seq {
        for n in 1 .. 999999 do
            if (isPalindrome n) && (isBinPalindrome n) then
                yield n
    }

let solution = numbers |> Seq.sum
printfn "solution: %d" solution

Console.ReadKey(true) |> ignore



