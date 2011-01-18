///Project Euler Problem 37
///The number 3797 has an interesting property. Being prime itself, it is possible to continuously 
///remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly 
///we can work from right to left: 3797, 379, 37, and 3.
///
///Find the sum of the only eleven primes that are both truncatable from left to right and right to 
///left.
///
///NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
//-----------------------------------------------------------------------
// <copyright file="truncprimes.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Text.RegularExpressions
open Euler

let limit = int 1E6

let primes,isPrime =
    let primes = primes |> Seq.takeWhile(fun p -> p < uint64 limit) |> Seq.map int |> List.ofSeq

    let primeTable = 
            let table = Array.create (limit+1) false
            primes |> Seq.iter (fun p -> table.[p] <- true)
            table
    primes, (fun n -> primeTable.[n])

let truncateLeft (n :int) = 
                let arr = n.ToString().Remove(0,1).ToCharArray()
                Convert.ToInt32(new String(arr))

let truncateRight (n :int) = 
                let arr = n.ToString().Remove(n.ToString().Length - 1,1).ToCharArray()
                Convert.ToInt32(new String(arr))

let isTruncatablePrime = 
    let regex = new Regex(@"[0468]")
    fun n ->
        let str = n.ToString()
        let m = regex.Match(str)
        if  n < 10 || m.Success then
            false
        else
            let length = n.ToString().Length
            let result = ref true
            let truncL = ref n
            let truncR = ref n
            for i in 0 .. length - 2 do
                truncR := truncateRight !truncR
                truncL := truncateLeft !truncL
                result := !result && (isPrime !truncR) && (isPrime !truncL) 
            !result

let circular = primes |> Seq.filter(isTruncatablePrime)
//circular |> Seq.iter(printfn "%d")
let solution = circular |> Seq.sum
printfn "solution: %d" solution

Console.ReadKey(true) |> ignore

