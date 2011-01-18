///Project Euler Problem 6
///The sum of the squares of the first ten natural numbers is,
///                 1^2 + 2^2 + ... + 10^2 = 385
///
///The square of the sum of the first ten natural numbers is,
///                (1 + 2 + ... + 10)^2 = 552 = 3025
///
///Hence the difference between the sum of the squares of the first ten natural 
///numbers and the square of the sum is 3025 385 = 2640.
///
///Find the difference between the sum of the squares of the 
///first one hundred natural numbers and the square of the sum.
//-----------------------------------------------------------------------
// <copyright file="squares.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let limit = 100
let numbers = [1..limit]

let flip f a b = f b a

let squareOfSum = numbers |> List.sum |> flip pown 2

let sumOfSquares = numbers |> List.sumBy(fun n -> n*n)

squareOfSum - sumOfSquares |> printfn "solution: %d"
Console.ReadKey(true) |> ignore