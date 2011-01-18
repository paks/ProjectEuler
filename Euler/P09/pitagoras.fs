///Project Euler Problem 9
///A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
///                         a^2 + b^2 = c^2
///
///For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
///
///There exists exactly one Pythagorean triplet for which a + b + c = 1000.
///Find the product abc.
//-----------------------------------------------------------------------
// <copyright file="pitagoras.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let pitagoras = 
    seq {
         for a in 1 .. (1000 - 3)/3 do
            for b in a+1 .. (1000-1-a)/2 do
                let c = 1000-a-b
                if a*a+b*b=c*c then
                    yield (a,b,c)
    }
    
let (a,b,c) = pitagoras |> Seq.head 
a*b*c |> printfn "solution: %d"

Console.ReadKey(true) |> ignore