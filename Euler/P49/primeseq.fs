///Project Euler Problem 11
///The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is 
///unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers 
///are permutations of one another.
///
///There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this 
///property, but there is one other 4-digit increasing sequence.
///
///What 12-digit number do you form by concatenating the three terms in this sequence?
//-----------------------------------------------------------------------
// <copyright file="primeseq.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open Euler

let candidates = 
    let primes4d = primes |> Seq.skipWhile(fun p -> p < 999UL) |> Seq.takeWhile(fun p -> p < 10000UL) |> Seq.map int

    let table = Array.create 10000 ([] : int list)

    let sortDigits (n : int) = int (new String(n.ToString().ToCharArray() |> Array.sort))


    primes4d |> Seq.iter(fun p -> 
                         let sp = sortDigits p
                         table.[sp] <- table.[sp] @ [p] )
    table |> Array.toList |> List.filter(fun l -> l.Length > 2)

let isSeries = function
        | []  -> false
        | [_] -> false
        | [_;_] -> false
        | (l : int list) ->
                    let arr = l |> List.toArray
                    let k = arr.[1] - arr.[0]
                    let k' = arr.[2] - arr.[1]
                    k = k'

let permutations  (l : int list) = 
        let permutations' (l : int array) = 
            let mutable result = []
            let n = l.Length
            for i in 0 .. n - 3 do
                for j in i+1 .. n - 2 do
                   for k in j+1 .. n - 1 do
                        let p = l.[i]::l.[j]::l.[k]::[]
                        result <- p::result
            result
        match l with
            | []  -> [[]]
            | [_] -> [[]]
            | [_;_] -> [[]]
            | [_;_;_] -> [l]
            | l                   -> permutations' (l |> List.toArray)

//[1..6] |> permutations |> Seq.iter (printfn "%A")
let solution = candidates 
               |> List.map(fun l -> permutations l |> List.filter(isSeries)) 
               |> List.filter(List.isEmpty >> not) 
               |> List.rev 
               |> List.head 
               |> List.head
               |> List.fold(fun acc n -> acc + n.ToString()) ""
solution |> printfn "solution: %s"

Console.ReadKey(true) |> ignore

