///Project Euler Problem 60
///The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating 
///them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 
///1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four 
///primes with this property.
///
///Find the lowest sum for a set of five primes for which any two primes concatenate to produce 
///another prime.
//-----------------------------------------------------------------------
// <copyright file="concatprimes.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Numerics
open Euler2

let (++) (a :bigint) (b :bigint) = BigInteger.Parse (a.ToString() + b.ToString())

let toBigInt (n :uint64) = bigint n

let primes = primes |> Seq.takeWhile(fun p -> p < 10000UL) |> Seq.map toBigInt |> Seq.toList

let primesComb p = List.filter(fun pn -> p  ++ pn |> isPrime && pn ++ p |> isPrime)
    
let solutions = 
    seq {
        for a in primes do
            let la = primes |> List.filter(fun p -> p > a)
            let pca = la |> primesComb a 
            for b in pca do
                let lb = pca |> List.filter(fun p -> p > b)
                let pcb = lb |> primesComb b
                for c in pcb do
                    let lc = pcb |> List.filter(fun p -> p > c)
                    let pcc = lc |> primesComb c
                    for d in pcc do
                        let ld = pcc |> List.filter(fun p -> p > d)
                        let pcd = ld |> primesComb d
                        for e in pcd do
                            yield [a;b;c;d;e]
    }

let solution = solutions |> Seq.head
solution |> printfn "primes: %A"
solution |> Seq.sum |> printfn "solution: %A"

Console.ReadKey(true) |> ignore



