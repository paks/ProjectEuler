///Project Euler Problem 85
///A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a fly, F, sits in the 
///opposite corner. By travelling on the surfaces of the room the shortest "straight line" distance 
///from S to F is 10 and the path is shown on the diagram.
///
///However, there are up to three "shortest" path candidates for any given cuboid and the shortest 
///route is not always integer.
///
///By considering all cuboid rooms up to a maximum size of M by M by M, there are exactly 2060 
///cuboids for which the shortest distance is integer when M=100, and this is the least value of M 
///for which the number of solutions first exceeds two thousand; the number of solutions is 1975 
///when M=99.
///
///Find the least value of M such that the number of solutions first exceeds one million.
//-----------------------------------------------------------------------
// <copyright file="cuboid.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic

let isSquare x = 
    let x' = int ((sqrt(float x)))
    x = x'*x'

/// see http://mathworld.wolfram.com/SpiderandFlyProblem.html
let cube m = 
    let mm = m*m
    let result = ref 0
    for x in 1 .. m do
        for y in x .. m do
            let xy = x + y
            if (xy*xy + mm) |> isSquare then
                incr result
    !result

let limit = 1000000
let solutions n = seq { 1 .. n } |> Seq.map cube  |> Seq.scan (+) 0 |> Seq.findIndex(fun n -> n > limit)
solutions 2000 |> printfn "solution: %d"  //let's hope that the solution is below 2000

Console.ReadKey(true) |> ignore
