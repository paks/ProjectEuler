///Project Euler Problem 75
///It turns out that 12 cm is the smallest length of wire can be bent to form a right angle triangle in 
///exactly one way, but there are many more examples.
///
///        12 cm: (3,4,5)
///        24 cm: (6,8,10)
///        30 cm: (5,12,13)
///        36 cm: (9,12,15)
///        40 cm: (8,15,17)
///        48 cm: (12,16,20)
///
///In contrast, some lengths of wire, like 20 cm, cannot be bent to form a right angle triangle, and 
///other lengths allow more than one solution to be found; for example, using 120 cm it is possible 
///to form exactly three different right angle triangles.
///
///        120 cm: (30,40,50), (20,48,52), (24,45,51)
///
///Given that L is the length of the wire, for how many values of L 1,500,000 can exactly one 
///right angle triangle be formed?
//-----------------------------------------------------------------------
// <copyright file="wire.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open System.Numerics
//open 

/// these matrices comes from this site http://mathworld.wolfram.com/PythagoreanTriple.html

//org * U |> printfn "%A"
//org * A|> printfn "%A"
//org * D |> printfn "%A"

let limit = 1500000
let solutions = Array.create (limit+1) 0

///primitive computes all the primitive pythagorean triplets
let primitives (p :RowVector<float>) =
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
    
    let underlimit (p :RowVector<float>) = 
                            let a = p.[0]
                            let b = p.[1]
                            let c = p.[2]
                            let p = a +  b + c
                            p < float limit

    let rec primitives' (start :RowVector<float>) acc cont = 
                        if not (underlimit start) then
                            cont acc
                        else
                            let p1,p2,p3 = calcPythaTriplet start
                            let acc' = start::acc
                            primitives' p1 acc' (fun p1Acc -> primitives' p2 p1Acc (fun p2Acc -> primitives' p3 p2Acc cont))
    primitives' p [] id
    
let org = rowvec [| 3.; 4.; 5. |]
let alltriplets = primitives org |> Seq.map(fun row -> 
                                            let a = int (row.[0])
                                            let b = int (row.[1])
                                            let c = int (row.[2])
                                            let p = a + b + c
                                            if a < b then
                                                (p,(a,b,c))
                                            else
                                                (p,(b,a,c)))

alltriplets |> Seq.iter(fun (p,_) -> 
                                for np in p .. p .. limit do
                                    solutions.[np] <- solutions.[np] + 1)

solutions |> Seq.filter(fun p -> p = 1) |> Seq.length |> printfn "solution: %d"

Console.ReadKey(true) |> ignore