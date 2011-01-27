///Project Euler Problem 88
///A natural number, N, that can be written as the sum and product of a given set of at least two 
///natural numbers, {a1, a2, ... , ak} is called a product-sum number: N = a1 + a2 + ... + ak = a1 x a2 x ... x ak.
///
///For example, 6 = 1 + 2 + 3 = 1 x 2 x 3.

///For a given set of size, k, we shall call the smallest N with this property a minimal product-sum 
///number. The minimal product-sum numbers for sets of size, k = 2, 3, 4, 5, and 6 are as follows.
///
///                k=2: 4 = 2 x 2 = 2 + 2
///                k=3: 6 = 1 x 2 x 3 = 1 + 2 + 3
///                k=4: 8 = 1 x 1 x 2 x 4 = 1 + 1 + 2 + 4
///                k=5: 8 = 1 x 1 x 2 x 2 x 2 = 1 + 1 + 2 + 2 + 2
///                k=6:12 = 1 x 1 x 1 x 1 x 2 x 6 = 1 + 1 + 1 + 1 + 2 + 6
///
///Hence for 2<=k<=6, the sum of all the minimal product-sum numbers is 4+6+8+12 = 30; note that 8 
///is only counted once in the sum.
///
///In fact, as the complete set of minimal product-sum numbers for 2<=k<=12 is {4, 6, 8, 12, 15, 16}, 
///the sum is 61.
///
///What is the sum of all the minimal product-sum numbers for 2 <= k <= 12000?
//-----------------------------------------------------------------------
// <copyright file="productsum.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Diagnostics
let sw = new Stopwatch()
sw.Start()

let maxK = 12000
let maxN = 2 * maxK

let limit = int (sqrt(float maxN)) + 1

// printfn "limit %d" limit
// TODO: Work on a faster solution
let products (size :int) maxK = 
    let limit = int ((float maxN)**(1./(float size))) + size
    let rec products' max limit prod sum =
        seq {
            for a in 2 .. limit do
                if max = size then
                    let pr = a*prod
                    let sum = a + sum
                    let k = size + (pr - sum)
                    if pr <= maxN  &&  k <= maxK then
                        yield (pr, k)
                else
                    let product = a*prod
                    let sum = a + sum
                    let nextLim = (maxN/product) + 1
                    yield! products' (max + 1) nextLim product sum
        }
    products' 1 limit 1 0
    
let solution = 
    let solutions = Array.create (maxK + 1) 0
    for km in 2 .. 13 do
        for (pr, k) in products km maxK do
                let sol = solutions.[k]
                if sol = 0 || pr < sol then
                    solutions.[k] <- pr
    solutions |> Seq.distinct |>Seq.sum

solution |> printfn "solution: %d"
sw.Stop()
sw.Elapsed |> printfn "%A"

Console.ReadKey(true) |> ignore
