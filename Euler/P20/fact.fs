///Project Euler Problem 20
///n! means n x (n - 1)  ...  3 x 2 x 1
///
///Find the sum of the digits in the number 100!
//-----------------------------------------------------------------------
// <copyright file="fact.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let factorial n =
    let rec loop n acc =
        match n with
            | _ when n = 0I -> acc
            | _ -> loop (n-1I) (n*acc)
    loop n 1I
    
(factorial 100I).ToString().ToCharArray() 
    |> Array.map (fun c -> int c - int '0')
    |> Array.sum 
    |> printfn "solution: %d"
Console.ReadKey(true) |> ignore