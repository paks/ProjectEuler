///Project Euler Problem 35
///The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, 
///are themselves prime.
///
///There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
///
///How many circular primes are there below one million?
//-----------------------------------------------------------------------
// <copyright file="circularprimes.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open Euler

let limit = int 1E6

let primes,isPrime =
    let primes = primes |> Seq.takeWhile(fun p -> p < uint64 limit) |> Seq.map int |> List.ofSeq

    let primeTable = 
            let table = Array.create (limit+1) false
            primes |> Seq.iter (fun p -> table.[p] <- true)
            table
    primes, (fun n -> primeTable.[n])


let rotate (n :int) = 
                let arr = n.ToString().ToCharArray()
                let tmp = arr.[0]
                let length = arr.Length
                match length with
                    | 1 -> arr.[0] <- tmp
                    | 2 -> arr.[0] <- arr.[1]
                           arr.[1] <- tmp
                    | _ -> for i in 0 .. arr.Length - 2 do
                                arr.[i] <- arr.[i+1]
                           arr.[arr.Length - 1] <- tmp
                Convert.ToInt32(new String(arr))

let isCircularPrime (n :int) = 
                        let str = n.ToString()
                        if  str.Contains("0") then
                            false
                        else
                            let length = n.ToString().Length
                            let result = ref true
                            let rotated = ref n
                            for i in 0 .. length - 1 do
                                result := !result && (isPrime !rotated)
                                rotated := rotate !rotated
                            !result

let circular = primes |> Seq.filter isCircularPrime
//circular |> Seq.iter(printfn "%d")
let solution = circular |> Seq.length
printfn "solution: %d" solution

Console.ReadKey(true) |> ignore

