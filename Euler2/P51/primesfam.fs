///Project Euler Problem 51
///By replacing the 1st digit of *57, it turns out that six of the possible values: 157, 257, 457, 557, 
///757, and 857, are all prime.
///
///By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first 
///example having seven primes, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 
///56993. Consequently 56003, being the first member of this family, is the smallest prime with this 
///property.
///
///Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) 
///with the same digit, is part of an eight prime value family.
//-----------------------------------------------------------------------
// <copyright file="primesfam.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open Euler2
open System.Diagnostics

let limit = int 1E6

let primes,isPrime = 
    let primes = primes |> Seq.takeWhile(fun p -> p < uint64 limit) |> Seq.map int

    let primeTable = 
            let table = Array.create (limit+1) false
            primes |> Seq.iter (fun p -> table.[p] <- true)
            table

    primes, fun n -> primeTable.[n]

let replaceOnes (a: int) b = 
            let arr = a.ToString().ToCharArray() |> Array.map(fun ch -> if ch = '1' then b else ch)
            Convert.ToInt32(new String(arr))

let numOnes (a :int) = a.ToString().ToCharArray() |> Seq.sumBy(fun ch -> if ch = '1' then 1 else 0)

///replaceOnes 521113 '3' |> printfn "replaceOnes 521113 : %d"
///numOnes 521113 |> printfn "numOnes 521113 : %d"

let candidates = 
    seq {
        for n in 100000 .. 999999 do
            if numOnes n = 3 then
                yield n
    }

let primeSeq n =
    [
        for ch in '1' .. '9' do
            let p = replaceOnes n ch
            if p |> isPrime then
                yield p
    ]

let solutions = candidates |> Seq.map(fun c -> primeSeq c) |> Seq.filter(fun s -> s.Length = 8)
solutions |> Seq.head |> Seq.head|> printfn "solution: %d"

Console.ReadKey(true) |> ignore
