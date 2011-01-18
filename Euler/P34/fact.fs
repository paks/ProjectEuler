///Project Euler Problem 34
///145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
///
///Find the sum of all numbers which are equal to the sum of the factorial of their digits.
///
///Note: as 1! = 1 and 2! = 2 are not sums they are not included.
//-----------------------------------------------------------------------
// <copyright file="fact.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Numerics

let factorial n =
    let rec loop n acc =
        match n with
            | 0 -> acc
            | _ -> loop (n-1) (n*acc)
    loop n 1

let table = 
        [| 
            yield 1 
            for n in 1 .. 9 do
                yield (factorial n) |]

let digits n = n.ToString().ToCharArray() |> Array.map(fun c -> (int c) - (int '0') ) |> Array.toList

let sumFact n = n |> digits |> Seq.sumBy(fun d -> table.[int d] )

//let fact9 = BigInt.factorial 9I
//printfn "%d : %A" 145 (sumFact (145I))
//let toString (n : BigInt) = n.ToString()
//(toString 34I) |> printfn "%s" 

let numb = 
    [
        for n in 10 .. 2540160 do //9,999,999 > 2,540,160
            let s = sumFact n
            if n = s then
                yield n
    ]
    
//numb |> Seq.iter(printfn "%A")
let solution = numb |> Seq.sum
solution |> printfn "solution: %A"
Console.ReadKey(true) |> ignore