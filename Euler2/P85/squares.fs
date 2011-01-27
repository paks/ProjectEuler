///Project Euler Problem 85
///By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains eighteen 
///rectangles:
///
///                  http://projecteuler.net/index.php?section=problems&id=85
///
///Although there exists no rectangular grid that contains exactly two million rectangles, find the 
///area of the grid with the nearest solution.
//-----------------------------------------------------------------------
// <copyright file="squares.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let rectangles a b = (a*(a+1)*b*(b+1))/4
let limit = int 2E6
let solution = 
    let rec solution' a b =
        let rect = rectangles a b
        if rect >= limit then
            a*b
        else
            if b <= 100 then // lets hope the solution doesn't require b > 100
                solution' a (b+1)
            else
                solution' (a+1) 1
    solution' 1 1

solution |> printfn "solution: %d"
Console.ReadKey(true) |> ignore

