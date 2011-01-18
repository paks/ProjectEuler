///Project Euler Problem 46
///It was proposed by Christian Goldbach that every odd composite number can be written as the 
///sum of a prime and twice a square.
///
///  9 =  7 + 2x1^2
/// 15 =  7 + 2x2^2
/// 21 =  3 + 2x3^2
/// 25 =  7 + 2x3^2
/// 27 = 19 + 2x2^2
/// 33 = 31 + 2x1^2
///
///It turns out that the conjecture was false.
///
///What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
//-----------------------------------------------------------------------
// <copyright file="goldbach.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open Euler

let limit = int 1E5

let primes,isPrime =
    let primes = primes |> Seq.takeWhile(fun p -> p < uint64 limit) |> Seq.map int |> List.ofSeq

    let primeTable = 
            let table = Array.create (limit+1) false
            primes |> Seq.iter (fun p -> table.[p] <- true)
            table
    primes, (fun n -> primeTable.[n])

let squares n = (1,n) |> Seq.unfold(fun (next,n) -> let square = next*next
                                                    if 2*square < n then
                                                        Some (square, (next+1,n) )
                                                    else
                                                        None )
//squares 12 |> Seq.iter(printfn "%d")
let oddsNotPrime =
    let rec odds' n = seq {
        if n |> isPrime |> not then
            yield n
        yield! odds' (n+2)
    }
    odds' 9

let isGoldbach n = squares n |> Seq.exists (fun sq -> n - 2*sq |> isPrime)
        
let solution = oddsNotPrime |> Seq.find(isGoldbach >> not)
printfn "solution: %d" solution

Console.ReadKey(true) |> ignore