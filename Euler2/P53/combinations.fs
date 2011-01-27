///Project Euler Problem 52
///There are exactly ten ways of selecting three from five, 12345:
///
///123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
///
///In combinatorics, we use the notation, 5C3 = 10.
///
///In general,
///nCr =  n! / r!(n-r)!	,where r n, n! = n(n1)...321, and 0! = 1.
///
///It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
///
///How many values of  nCr, for 1 n 100, are greater than one-million?
//-----------------------------------------------------------------------
// <copyright file="combinations.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Diagnostics

let fact a b = seq { a .. b } |> Seq.fold(( * )) 1I

let C n r = (fact (r+1I) n) / (fact 1I (n - r))

let millions = 
    seq {
        for n in 1I .. 100I do
            for r in 1I .. n do
                let comb = C n r
                if comb > 1000000I then
                    yield (n,r)
                
    }
    
millions |> Seq.length |> printfn "%d"

Console.ReadKey(true) |> ignore