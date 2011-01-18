///Project Euler Problem 40
///An irrational decimal fraction is created by concatenating the positive integers:
///
///0.12345678910(1)112131415161718192021...
///
///It can be seen that the 12th digit of the fractional part is 1.
///
///If dn represents the nth digit of the fractional part, find the value of the following expression.
///
///d1 x d10 x d100 x d1000 x d10000 x d100000 x d1000000
//-----------------------------------------------------------------------
// <copyright file="irrational.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let fraction = Array.create 1000001 0
let next = ref 0

let toArray (n : int) = n.ToString().ToCharArray() |> Array.map (fun c -> (int c) - (int '0'))

for i in 0 .. 190000 do
    let number = toArray (i + 1)
    let start = !next
    for j in 0 .. number.Length - 1 do
        if start + j < 1000000 then
            fraction.[start + j] <- number.[j]
    next := !next + number.Length

// fraction |> printfn "%A"

fraction.[0] * fraction.[9] * fraction.[99] * fraction.[999] * fraction.[9999] * fraction.[99999] * fraction.[999999] |> printfn "solution: %d"

Console.ReadKey(true) |> ignore