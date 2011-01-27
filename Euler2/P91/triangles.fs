///Project Euler Problem 91
///The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and are joined to the 
///origin, O(0,0), to form ΔOPQ.
///
///There are exactly fourteen triangles containing a right angle that can be formed when each 
///co-ordinate lies between 0 and 2 inclusive; that is, 
///0 x1, y1, x2, y2 2.
///
///Given that 0 <= x1, y1, x2, y2 <= 50, how many right triangles can be formed?
//-----------------------------------------------------------------------
// <copyright file="triangles.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let points limit = 
    let point =
        [
            for x in 0 .. limit do
                for y in 0 .. limit do
                    if (x,y) <> (0,0) then
                        yield x,y
        ]
    seq {
        for p1 in point do
            for p2 in point do
                if p1 <> p2 then
                    let x1,y1 = p1
                    let x2,y2 = p2
                    if x1 * (limit + 1) + y1 < x2 * (limit + 1) + y2 then
                        yield p1,p2
    }
    
let isValid (p1,p2) =
    let x1,y1 = p1
    let x2,y2 = p2
    
    let a = x1*x1 + y1*y1
    let b = x2*x2 + y2*y2
    let c = (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)
    (a+b=c) || (a+c=b) || (b+c=a)

let limit = 50
let solution = points limit |> Seq.filter(isValid) |> Seq.length
solution |> printfn "solution: %d"

Console.ReadKey(true) |> ignore


