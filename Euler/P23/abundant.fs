///Project Euler Problem 23
///A perfect number is a number for which the sum of its proper divisors is exactly equal to the 
///number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, 
///which means that 28 is a perfect number.
///
///A number whose proper divisors are less than the number is called deficient and a number whose 
///proper divisors exceed the number is called abundant.
///
///As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be
///written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown 
///that all integers greater than 28123 can be written as the sum of two abundant numbers. 
///However, this upper limit cannot be reduced any further by analysis even though it is known 
///that the greatest number that cannot be expressed as the sum of two abundant numbers is less 
///than this limit.
///
///Find the sum of all the positive integers which cannot be written as the sum of two abundant 
///numbers.
//-----------------------------------------------------------------------
// <copyright file="abundant.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open Euler

let tau p n = (Math.Pow(p,n+1.)- 1.)/(p - 1.)
let dn n = (primeFactors n |> Seq.fold (fun acc (p,n) -> (tau p (float n)) * acc) 1.) - n

//dn 36. |> printfn "%f"
//dn 28. |> printfn "%f"
//dn 12. |> printfn "%f"

let abundant = [|
                    for i in 12 .. 28123 do
                        let d = dn (float i)
                        if d > float i then
                            yield i
               |]


let composite = [|
                    for i in 0 .. abundant.Length - 1 do 
                        for j in i .. abundant.Length - 1 do
                            let result = abundant.[i] + abundant.[j]
                            if result < 28124 then
                                yield result
                |]

//printfn "abundant length: %d" abundant.Length

//let lastAbundant = abundant.[abundant.Length - 1]
//printfn "lastAbundant: %d" lastAbundant
//
//printfn "composite length: %d" composite.Length
//let lastComposite = composite.[composite.Length - 1]
//printfn "lastComposite: %d" lastComposite

let memo = Array.create (composite.Length + 1) 0

for i in 0 .. composite.Length - 1 do
    let comp = composite.[i]
    memo.[comp] <- comp
    

let sum = [1 .. 28123] |> List.filter (fun n -> memo.[n] = 0) |> List.sum

printfn "solution: %d" sum

Console.ReadKey(true) |> ignore


