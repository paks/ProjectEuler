///Project Euler Problem 50
///The prime 41, can be written as the sum of six consecutive primes:
///                          41 = 2 + 3 + 5 + 7 + 11 + 13
///
///This is the longest sum of consecutive primes that adds to a prime below one-hundred.
///
///The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 
///terms, and is equal to 953.
///
///Which prime, below one-million, can be written as the sum of the most consecutive primes?
//-----------------------------------------------------------------------
// <copyright file="primes.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open Euler

let limit = int 1E6

let primes = primes |> Seq.takeWhile(fun p -> p < 1000000UL) |> Seq.map int |> Array.ofSeq

let primeTable = 
        let table = Array.create (limit+1) false
        primes |> Seq.iter (fun p -> table.[p] <- true)
        table

let isPrime n = primeTable.[n]

let list = (0, 0) |> Seq.unfold(fun (next, sum) -> 
                            let prime = primes.[next]
                            if prime + sum > limit then
                                None
                            else
                                Some (prime, (next + 1,prime + sum))
                            ) |> Seq.toList

//list |> Seq.iter(printfn "%d")
//list |> Seq.sum |>printfn "%d"

let rec reduce  = function
    | [] -> []
    | list ->
        if list |> Seq.sum |> isPrime then
            list
        else
            list |> List.tail |> reduce

let solution1 = list |> reduce
let sum1 = solution1 |> Seq.sum

let solution2 = list |> List.rev |> reduce
let sum2 = solution2 |> Seq.sum

if sum1 > sum2 then
    //solution1 |> Seq.iter(printfn "%d")
    sum1 |> printfn "solution: %d"
else
    //solution2 |> Seq.iter(printfn "%d")
    sum2 |> printfn "solution: %d"

Console.ReadKey(true) |> ignore