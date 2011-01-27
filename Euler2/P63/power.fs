///Project Euler Problem 63
///The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9,
///is a ninth power.
///
///How many n-digit positive integers exist which are also an nth power?
//-----------------------------------------------------------------------
// <copyright file="power.fs" >
// Copyright © 2011 Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Numerics
open System.Diagnostics

let powers = (1I,1) 
             |> Seq.unfold(fun (number, power) ->
                    let digits (n: bigint) = n.ToString().Length
                    let n = pown number power
                    if digits n = power then
                        Some((n,(number,power)), (number+1I,power))
                    elif digits n < power then
                        Some((-1I,(0I,0)), (number+1I,power))
                    else
                        if power > 21 then
                            None
                        else
                            Some((-1I,(0I,0)), (1I,power+1))
                ) 
             |> Seq.filter(fst >> (<>) -1I)
                    
//powers |> Seq.iter(printfn "%A")
powers |> Seq.length |> printfn "solution: %d"

Console.ReadKey(true) |> ignore
