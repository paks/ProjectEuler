///Project Euler Problem 94
///It is easily proved that no equilateral triangle exists with integral length sides and integral area. 
///However, the almost equilateral triangle 5-5-6 has an area of 12 square units.
///
///We shall define an almost equilateral triangle to be a triangle for which two sides are equal and 
///the third differs by no more than one unit.
///
///Find the sum of the perimeters of every almost equilateral triangle with integral side lengths 
///and area and whose perimeters do not exceed one billion (1,000,000,000).
//-----------------------------------------------------------------------
// <copyright file="equilateral.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open System.Diagnostics
let sw = new Stopwatch()
sw.Start()

let limit = 1000000000L
///primitive computes all the primitive pythagorean triplets
let primitives =
    let to_tuple (row: rowvec) = if row.[0] < row.[1] then 
                                    (row.[0], row.[1], row.[2])
                                 else 
                                    (row.[1], row.[0], row.[2])
    let to_triangle row = 
        let a,b,c = row |> to_tuple 
        let p = 2.*c + 2.*a
        (p,(a,b,c))
            
    let isAlmostEquilateral triangle = 
        let a,_,c = triangle |> to_tuple
        c = 2. * a - 1. || c = 2. * a + 1.
      
    let org = rowvec [| 3.; 4.; 5. |]
    
    let U = matrix [| [|  1.; 2.; 2. |];
                      [| -2.;-1.;-2.|];
                      [|  2.; 2.; 3. |];|]

    let A = matrix [| [|  1.; 2.; 2. |];
                      [|  2.; 1.; 2.|];
                      [|  2.; 2.; 3. |];|]

    let D = matrix [| [| -1.;-2.;-2. |];
                      [|  2.; 1.; 2.|];
                      [|  2.; 2.; 3. |];|]

    let calcPythaTriplet (p: rowvec) = (p*U,p*A,p*D)
    
    let isAboveLimit (p :RowVector<float>) = 
                            let a = p.[0]
                            let b = p.[1]
                            let c = p.[2]
                            let p = a +  b + c
                            p > float limit

    let rec primitives' start cont = 
                    seq {
                        if isAboveLimit start then
                            yield! cont
                        else
                            let p1,p2,p3 = calcPythaTriplet start
                            if start |> isAlmostEquilateral then
                                let t = start |> to_triangle
                                yield t
                            yield! primitives' p1 (primitives' p2 (primitives' p3 cont))
                        }
    primitives' org (Seq.empty)

let solution = primitives |> Seq.sumBy fst
sw.Stop()
sw.Elapsed |> printfn "%A"

Console.ReadKey(true) |> ignore
